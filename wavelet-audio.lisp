(in-package :wavelet-audio)

(declaim (type (ub 16) *block-size*))
(defparameter *block-size* 1024)
(defparameter +current-version+ 1)
(defparameter +initial-version+ 1)
(defparameter +max-version+ 1)

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

(defun min-position (seq)
  (let ((min (reduce #'min seq)))
    (position min seq)))

(defun optimal-rice-parameter (seq start end)
  (min-position
   (loop for m from 0 below 20 collect 
        (let ((*count* 0))
          (loop for i from start below end do (write-rice nil (aref seq i) m))
          *count*))))

(defun write-wavelet-audio-header (stream metadata)
  "Write identifier and metadata to the stream. This function must
be called first to newly created stream."
  (declare (type bit-output-stream stream))
  ;; Write identifier (`WaVe')
  (write-octet #x57 stream)
  (write-octet #x61 stream)
  (write-octet #x56 stream)
  (write-octet #x65 stream)
  (write-metadata stream metadata)
  metadata)

(defun encode-block (channels)
  "Encode channels and return wavelet audio block ready for writing."
  (declare (optimize (speed 3))
           (type list channels))
  (if (= (length (the list channels)) 2)
      (apply #'decorrelate-channels channels))
  (make-instance 'wavelet-audio-block :channels
                 (loop for channel in channels collect
                      (wavelet-forward-w/recopy (add-padding channel)))))

(defun write-block (stream wa-block &key block-number)
  "Write encoded block to the stream."
  (declare (optimize (speed 3))
           (type bit-output-stream stream)
           (type wavelet-audio-block wa-block))
  ;; Write sync code
  (write-octet #xff stream)
  (write-octet #xfe stream)
  ;; Write block number
  (write-block-number stream (or block-number (block-number wa-block)))

  (dolist (channel (block-channels wa-block))
    (declare (type (simple-array (sb 32)) channel))
    (let ((p (optimal-rice-parameter channel 0 2)))
      (write-bits p 5 stream)
      (write-rice stream (aref channel 0) p)
      (write-rice stream (aref channel 1) p))
    (loop
       for i from 1 below (1- (integer-length *block-size*))
       for start-idx = (ash 1 i)
       for end-idx = (ash 1 (1+ i))
       for p = (optimal-rice-parameter channel start-idx end-idx) do
         (write-bits p 5 stream)
         (loop for idx from start-idx below end-idx do
              (write-rice stream (aref channel idx) p))))
  (pad-to-byte-alignment 0 stream)
  wa-block)

(defun encode-wavelet-audio (input-name output-name)
  "Convert uncompressed wav file to wavelet-audio file"
  (with-open-file (input input-name :element-type '(unsigned-byte 8))
    (let* ((reader (wav:open-wav input))
           (header (wav:read-wav-header reader))
           (samples-num (wav:samples-num header))
           (padding-num (- (nth-value 1 (ceiling samples-num *block-size*))))
           (streaminfo (make-instance 'wavelet-audio-streaminfo
                                :version +current-version+
                                :samplerate (wav:format-samplerate (car header))
                                :channels (wav:format-channels-num (car header))
                                :bps (wav:format-bps (car header))
                                :samples (+ samples-num padding-num)
                                :block-size *block-size*)))
      (wav:reader-position-to-audio-data reader header)

      (with-open-file (output
                       output-name
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
        (with-bit-output-stream
            (s :callback (make-stream-output-callback output))
          (write-wavelet-audio-header s (list streaminfo))
          (loop
             for block-start below samples-num by *block-size*
             for data-read from 0 by *block-size*
             for block-count from 0 by 1
             for data = (wav:read-wav-data reader (car header)
                                           (min *block-size* (- samples-num data-read))
                                           :decompose t)
             do (write-block s (encode-block data)
                             :block-number block-count))
          (flush-bit-output-stream s)))))
  t)

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

(defun check-wavelet-audio (stream)
  (if (or (/= (read-octet stream) #x57)
          (/= (read-octet stream) #x61)
          (/= (read-octet stream) #x56)
          (/= (read-octet stream) #x65))
      (error 'wavelet-audio-error :format-control "Not a wavelet audio file")))

(defun read-block (stream streaminfo)
  "Read block from previously opened stream. STREAMINFO metadata block must be given."
  (declare (optimize (speed 3))
           (type wavelet-audio-streaminfo streaminfo)
           (type bit-input-stream stream))
  (if (or (/= (read-octet stream) #xff)
          (/= (read-octet stream) #xfe))
      (error 'wavelet-audio-frame-error :stream stream))

  (let* ((channels (streaminfo-channels streaminfo))
         (block-size (streaminfo-block-size streaminfo))
         (wa-block (make-instance 'wavelet-audio-block
                                  :streaminfo streaminfo
                                  :channels (loop repeat channels collect
                                                 (make-array block-size
                                                             :element-type '(signed-byte 32)))
                                  :number (read-block-number stream))))
    (declare (type (ub 8) channels)
             (type (ub 16) block-size))
    (dolist (channel (block-channels wa-block))
      (declare (type (simple-array (sb 32)) channel))
      (let ((p (read-bits 5 stream)))
        (setf (aref channel 0) (read-rice stream p))
        (setf (aref channel 1) (read-rice stream p)))
      (loop
         for i from 1 below (1- (integer-length block-size))
         for start-idx = (ash 1 i)
         for end-idx = (ash 1 (1+ i))
         for p = (read-bits 5 stream) do
           (loop for idx from start-idx below end-idx do
                (setf (aref channel idx) (read-rice stream p)))))
    (read-to-byte-alignment stream)
    wa-block))

(defun decode-block (wa-block)
  (declare (optimize (speed 3)))
  (let* ((channels (block-channels wa-block))
         (decoded-channels (mapcar #'wavelet-inverse-w/recopy channels)))
    (if (= (length (the list channels)) 2)
        (correlate-channels (first decoded-channels)
                            (second decoded-channels)))
    decoded-channels))

(defun open-wavelet-audio (stream)
  "Check if STREAM is wavelet audio stream and read metadata."
  (check-wavelet-audio stream)
  (let* ((metadata (read-metadata stream))
         (streaminfo (first metadata)))
    (if (not (typep (first metadata) 'wavelet-audio-streaminfo))
        (error 'wavelet-audio-error :format-control "No streaminfo in the stream"))
    (if (> (streaminfo-version streaminfo) +max-version+)
        (error 'wavelet-audio-error :format-control "Unsupported version"))
    metadata))

(defun decode-wavelet-audio (input-name output-name)
  "Decode wavelet-audio file into .wav file"
  (with-open-file (input input-name :element-type '(unsigned-byte 8))
    (with-bit-input-stream
        (s :callback (make-stream-input-callback input))
      (let* ((metadata (open-wavelet-audio s))
             (streaminfo (first metadata))
             (samplerate (streaminfo-samplerate streaminfo))
             (bps (streaminfo-bps streaminfo))
             (channels (streaminfo-channels streaminfo))
             (samples (streaminfo-samples streaminfo))
             (block-size (streaminfo-block-size streaminfo)))
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
                 (let ((decoded-bufs (decode-block (read-block s streaminfo))))
                   (utils:mixchannels out-buf decoded-bufs)
                   (write-sequence out-buf output))))))))
  t)
