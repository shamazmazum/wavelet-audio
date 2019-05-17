(in-package :wavelet-audio)

(declaim (type (ub 16) *block-size*))
(defparameter *block-size* 4096  "Audio block size in samples.")
(defparameter *history-size* 50 "History size for adaptive Rice coder")
(defconstant +current-version+ 2)
(defconstant +initial-version+ 1)
(defconstant +max-version+ 2)

;; Encoding

(defun decorrelate-channels (chan1 chan2)
  "Decorrelate audio channels in stereo stream."
  (declare (type (simple-array (signed-byte 32)) chan1 chan2)
           (optimize (speed 3)))
  (map-into chan2 #'- chan2 chan1)
  (map-into chan1 (lambda (x y)
                    (declare (type (signed-byte 32) x y))
                    (+ x (truncate y 2)))
            chan1 chan2)
  (values chan1 chan2))

(defun add-padding (array)
  "Add zero padding in the end of array to make its size power of two."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (sb 32)) array))
  (let ((len (length array)))
    (assert (<= len *block-size*))
    (let ((pad-elements (- *block-size* len)))
      (if (zerop pad-elements) array
          (make-array *block-size*
                      :element-type '(signed-byte 32)
                      :initial-contents (concatenate 'list array
                                                     (loop repeat pad-elements collect 0)))))))

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

(defun write-block (stream wa-block)
  "Write encoded block @cl:param(wa-block) to the stream
@cl:param(stream)."
  (declare (optimize (speed 3))
           (type bit-output-stream stream)
           (type wavelet-audio-block wa-block))
  ;; Write sync code
  (write-octet #xff stream)
  (write-octet #xfe stream)
  ;; Write block number
  (write-block-number stream (block-number wa-block))

  (dolist (channel (block-channels wa-block))
    (declare (type (simple-array (sb 32)) channel))
    (let ((history (make-history 2)))
      (adaptive-write history stream (aref channel 0))
      (adaptive-write history stream (aref channel 1)))
    (loop
       for i from 1 below (1- (integer-length *block-size*))
       for start-idx = (ash 1 i)
       for end-idx = (ash 1 (1+ i))
       for history = (make-history *history-size*) do
         (loop for idx from start-idx below end-idx do
              (adaptive-write history stream (aref channel idx)))))
  (pad-to-byte-alignment 0 stream)
  wa-block)

(defun encode-wavelet-audio (input-name output-name)
  "Convert uncompressed wav file with name @cl:param(input-name) to
wavelet-audio file with name @cl:param(output-name)."
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
                                :block-size *block-size*
                                :history-size *history-size*)))
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
             for wa-block = (encode-block data) do
               (setf (block-number wa-block) block-count)
               (write-block s wa-block))
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
  "Read block from previously opened stream
  @cl:param(stream). @cl:param(streaminfo) metadata block must be
  given."
  (declare (optimize (speed 3))
           (type wavelet-audio-streaminfo streaminfo)
           (type bit-input-stream stream))
  (if (or (/= (read-octet stream) #xff)
          (/= (read-octet stream) #xfe))
      (error 'wavelet-audio-frame-error :stream stream))

  (let* ((channels (streaminfo-channels streaminfo))
         (block-size (streaminfo-block-size streaminfo))
         (history-size (streaminfo-history-size streaminfo))
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
      (let ((history (make-history 2)))
        (setf (aref channel 0) (adaptive-read history stream)
              (aref channel 1) (adaptive-read history stream)))
      (loop
         for i from 1 below (1- (integer-length block-size))
         for start-idx = (ash 1 i)
         for end-idx = (ash 1 (1+ i))
         for history = (make-history history-size) do
           (loop for idx from start-idx below end-idx do
                (setf (aref channel idx) (adaptive-read history stream)))))
    (read-to-byte-alignment stream)
    wa-block))

(defun decode-block (wa-block)
  "Decode audio block."
  (declare (optimize (speed 3)))
  (let* ((channels (block-channels wa-block))
         (decoded-channels (mapcar #'wavelet-inverse-w/recopy channels)))
    (if (= (length (the list channels)) 2)
        (correlate-channels (first decoded-channels)
                            (second decoded-channels)))
    decoded-channels))

(defun open-wavelet-audio (stream)
  "Check if @cl:param(stream) is wavelet audio stream and read metadata."
  (check-wavelet-audio stream)
  (let* ((metadata (read-metadata stream))
         (streaminfo (first metadata)))
    (if (not (typep (first metadata) 'wavelet-audio-streaminfo))
        (error 'wavelet-audio-error :format-control "No streaminfo in the stream"))
    (if (> (streaminfo-version streaminfo) +max-version+)
        (error 'wavelet-audio-error :format-control "Unsupported version"))
    metadata))

(defun decode-wavelet-audio (input-name output-name)
  "Decode wavelet-audio file with name @cl:param(input-name) into .wav file
with name @cl:param(output-name)"
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
