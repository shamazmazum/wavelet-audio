(in-package :wavelet-audio)

(defparameter *block-size* 1024)

(declaim (type (integer 0 #.most-positive-fixnum) *count*))
(defvar *count* 0)

(define-condition wavelet-audio-condition (simple-condition)
  ()
  (:documentation "Generic wavelet-audio condition"))

(define-condition wavelet-audio-error (wavelet-audio-condition error)
  ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Wavelet-audio error: " (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "wavelet-audio error"))

(define-condition wavelet-audio-broken-header (wavelet-audio-error)
  ()
  (:default-initargs
   :format-control "Broken header")
  (:documentation "This condition is signaled when the stream is not wavelet-audio stream"))

(defstruct wavelet-audio-header
  (samplerate 0 :type (unsigned-byte 32))
  (bps 0 :type (unsigned-byte 8))
  (channels 0 :type (unsigned-byte 8))
  (samples 0 :type (unsigned-byte 32))
  (block-size 0 :type (unsigned-byte 16)))

;; Encoding

(defun decorrelate-channels (chan1 chan2)
  "Decorrelate audio channels in stereo stream"
  (declare (type (simple-array (signed-byte 32)) chan1 chan2)
           (optimize (speed 3)))
  (map-into chan2 #'- chan2 chan1)
  (map-into chan1 (lambda (x y)
                    (declare (type (signed-byte 32) x y))
                    (+ x (truncate y 2)))
            chan1 chan2)
  (values chan1 chan2))

(defun add-padding (array)
  "Add zero padding in the end of array to make its size power of two"
  (let* ((len (length array))
         (pad-elements (- (nth-value 1 (ceiling len *block-size*))))
         (padding (make-array pad-elements :initial-element 0))
         (content (concatenate 'list array padding)))
    (make-array (+ pad-elements len)
                :element-type '(signed-byte 32)
                :initial-contents content)))

;; Rice coding functions
(defun write-rice-unsigned (stream residual m)
  (declare (type (unsigned-byte 6) m)
           (type (unsigned-byte 32) residual)
           (optimize (speed 3)))
  (let ((quotient (ash residual (- m)))
        (remainder (logand residual (1- (ash 1 m)))))
    (incf *count* (+ quotient 1 m))
    (when stream
      (loop repeat quotient do (trivial-bit-streams:write-bit 1 stream))
      (trivial-bit-streams:write-bit 0 stream)
      (trivial-bit-streams:write-bits remainder m stream))))

(defun write-rice (stream residual m)
  (declare (optimize (speed 3))
           (type (signed-byte 32) residual))
  (write-rice-unsigned
   stream
   (+ (* 2 (abs residual))
      (if (< residual 0) 1 0))
   m))

(defun min-position (seq)
  (let ((min (reduce #'min seq)))
    (position min seq)))

(defun optimal-rice-parameter (seq start end)
  (min-position
   (loop for m from 0 below 20 collect 
        (let ((*count* 0))
          (loop for i from start below end do (write-rice nil (aref seq i) m))
          *count*))))

(defun encode-wavelet-audio (input-name output-name)
  "Convert uncompressed wav file to wavelet-audio file"
  (with-open-file (input input-name :element-type '(unsigned-byte 8))
    (let* ((reader (wav:open-wav input))
           (header (wav:read-wav-header reader))
           (samples-num (wav:samples-num header))
           (padding-num (- (nth-value 1 (ceiling samples-num *block-size*)))))
      (wav:reader-position-to-audio-data reader header)

      (with-open-file (output
                       output-name
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
        (trivial-bit-streams:with-bit-output-stream
            (s :callback (trivial-bit-streams:make-stream-output-callback output))

          ;; 'WaVe
          (trivial-bit-streams:write-octet 57 s)
          (trivial-bit-streams:write-octet 61 s)
          (trivial-bit-streams:write-octet 56 s)
          (trivial-bit-streams:write-octet 65 s)

          (trivial-bit-streams:write-bits (wav:format-samplerate (car header)) 32 s)
          (trivial-bit-streams:write-bits (wav:format-bps (car header)) 8 s)
          (trivial-bit-streams:write-bits (wav:format-channels-num (car header)) 8 s)
          (trivial-bit-streams:write-bits (+ samples-num padding-num) 32 s)
          (trivial-bit-streams:write-bits *block-size* 16 s)
          (loop
             for block-start below samples-num by *block-size*
             for data-read from 0 by *block-size*
             for block-count from 0 by 1
             for data = (wav:read-wav-data reader (car header)
                                           (min *block-size* (- samples-num data-read))
                                           :decompose t)
             do
               (if (= (length data) 2)
                   (decorrelate-channels (first data)
                                         (second data)))
               (loop for channel in data do
                    (setq channel (wavelet-forward-w/recopy (add-padding channel)))

                    (let ((p (optimal-rice-parameter channel 0 2)))
                      (trivial-bit-streams:write-bits p 5 s)
                      (write-rice s (aref channel 0) p)
                      (write-rice s (aref channel 1) p))
                    (loop
                       for i from 1 below (1- (integer-length *block-size*))
                       for start-idx = (ash 1 i)
                       for end-idx = (ash 1 (1+ i))
                       for p = (optimal-rice-parameter channel start-idx end-idx) do
                         (trivial-bit-streams:write-bits p 5 s)
                         (loop for idx from start-idx below end-idx do
                              (write-rice s (aref channel idx) p)))))
          (trivial-bit-streams:flush-bit-output-stream s))))))

;; Decoding

(defun correlate-channels (chan1 chan2)
  "Inverse DECORRELATE-CHANNELS"
  (declare (type (simple-array (signed-byte 32)) chan1 chan2)
           (optimize (speed 3)))
  (map-into chan1 (lambda (x y)
                    (declare (type (signed-byte 32) x y))
                    (- x (truncate y 2)))
            chan1 chan2)
  (map-into chan2 #'+ chan2 chan1)
  (values chan1 chan2))

(defun read-rice-unsigned (stream m)
  (let ((quotient
         (loop
            for bit = (trivial-bit-streams:read-bit stream)
            until (zerop bit)
            sum 1))
        (remainder (trivial-bit-streams:read-bits m stream)))
    (+ remainder (ash quotient m))))

(defun read-rice (stream m)
  (declare (optimize (speed 3)))
  (let* ((unsigned (read-rice-unsigned stream m))
         (res (ash unsigned -1)))
    (declare (type (unsigned-byte 32) unsigned res))
    (if (zerop (logand unsigned 1))
        res (- res))))

(defun read-wavelet-audio-header (stream)
  (if (or (/= (trivial-bit-streams:read-octet stream) 57)
          (/= (trivial-bit-streams:read-octet stream) 61)
          (/= (trivial-bit-streams:read-octet stream) 56)
          (/= (trivial-bit-streams:read-octet stream) 65))
      (error 'wavelet-audio-broken-header))
  (make-wavelet-audio-header
   :samplerate (trivial-bit-streams:read-bits 32 stream)
   :bps (trivial-bit-streams:read-bits 8 stream)
   :channels (trivial-bit-streams:read-bits 8 stream)
   :samples (trivial-bit-streams:read-bits 32 stream)
   :block-size (trivial-bit-streams:read-bits 16 stream)))

(defun decode-block (s channels block-size)
  (let ((channel-bufs (loop repeat channels collect
                           (make-array block-size :element-type '(signed-byte 32)))))
    (loop for channel in channel-bufs do
         (let ((p (trivial-bit-streams:read-bits 5 s)))
           (setf (aref channel 0) (read-rice s p))
           (setf (aref channel 1) (read-rice s p)))
         (loop
            for i from 1 below (1- (integer-length block-size))
            for start-idx = (ash 1 i)
            for end-idx = (ash 1 (1+ i))
            for p = (trivial-bit-streams:read-bits 5 s) do
              (loop for idx from start-idx below end-idx do
                   (setf (aref channel idx) (read-rice s p)))))
    (let ((decoded-bufs (mapcar #'wavelet-inverse-w/recopy channel-bufs)))
      (if (= (length decoded-bufs) 2)
          (correlate-channels (first decoded-bufs)
                              (second decoded-bufs)))
      decoded-bufs)))

(defun decode-wavelet-audio (input-name output-name)
  "Decode wavelet-audio file into .wav file"
  (with-open-file (input input-name :element-type '(unsigned-byte 8))
    (trivial-bit-streams:with-bit-input-stream
        (s :callback (trivial-bit-streams:make-stream-input-callback input))
      (let* ((header (read-wavelet-audio-header s))
             (samplerate (wavelet-audio-header-samplerate header))
             (bps (wavelet-audio-header-bps header))
             (channels (wavelet-audio-header-channels header))
             (samples (wavelet-audio-header-samples header))
             (block-size (wavelet-audio-header-block-size header)))
        (utils:with-output-to-wav (output output-name
                                          :supersede t
                                          :samplerate samplerate
                                          :channels channels
                                          :bps bps
                                          :totalsamples samples)
          (let ((out-buf (make-array (* block-size channels)
                                     :element-type '(signed-byte 32)
                                     :initial-element 0)))
            (loop repeat (/ samples block-size) do
                 (let ((decoded-bufs (decode-block s channels block-size)))
                   (utils:mixchannels out-buf decoded-bufs)
                   (write-sequence out-buf output))))))))
  t)

#+nil
(defun play-wavelet-audio (name)
  "Play wavelet-audio file using cl-oss"
  (with-open-file (input name :element-type '(unsigned-byte 8))
    (trivial-bit-streams:with-bit-input-stream
        (s :callback (trivial-bit-streams:make-stream-input-callback input))
      (let* ((header (read-wavelet-audio-header s))
             (samplerate (wavelet-audio-header-samplerate header))
             (bps (wavelet-audio-header-bps header))
             (channels (wavelet-audio-header-channels header))
             (samples (wavelet-audio-header-samples header))
             (block-size (wavelet-audio-header-block-size header)))
        (if (/= bps 16) (error 'wavelet-audio-error :format-control "Cannot play, bits per second value is not 16"))
        (oss:with-dsp-device (output oss:dsp-device-output
                                     :sample-format oss:+afmt-s16-le+
                                     :channels channels
                                     :sample-rate samplerate)
          (let ((out-buf (make-array (* block-size channels)
                                     :element-type '(signed-byte 32)
                                     :initial-element 0)))
            (loop repeat (/ samples block-size) do
                 (let ((decoded-bufs (decode-block s channels block-size)))
                   (utils:mixchannels out-buf decoded-bufs)
                   (write-sequence out-buf output))))))))
  t)
