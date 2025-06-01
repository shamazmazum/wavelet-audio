(in-package :wavelet-audio)

;; Encoding

(sera:-> decorrelate-channels ((sa-sb 32) (sa-sb 32))
         (values (sa-sb 32) (sa-sb 32) &optional))
(defun decorrelate-channels (chan1 chan2)
  "Decorrelate audio channels in stereo stream."
  (declare (optimize (speed 3)))
  (map-into chan2 #'- chan2 chan1)
  (map-into
   chan1 (lambda (x y)
           (+ x (truncate y 2)))
   chan1 chan2)
  (values chan1 chan2))

(sera:-> add-padding ((sa-sb 32) (ub 16))
         (values (sa-sb 32) &optional))
(defun add-padding (array block-size)
  "Add zero padding in the end of array to make its size power of two."
  (declare (optimize (speed 3)))
  (let ((len (length array)))
    (assert (<= len block-size))
    (let ((pad-elements (- block-size len)))
      (if (zerop pad-elements) array
          (let ((padded (make-array block-size :element-type '(signed-byte 32))))
            (replace padded array)
            (fill padded 0 :start len)
            padded)))))

(sera:-> write-wavelet-audio-header (bit-output-stream list)
         (values list &optional))
(declaim (inline write-wavelet-audio-header))
(defun write-wavelet-audio-header (stream metadata)
  "Write identifier and metadata to the stream. This function must
be called first to newly created stream."
  ;; Write identifier (`WaVe')
  (write-octet #x57 stream)
  (write-octet #x61 stream)
  (write-octet #x56 stream)
  (write-octet #x65 stream)
  (write-metadata stream metadata)
  metadata)

(sera:-> encode-block (wavelet-audio-streaminfo list)
         (values wavelet-audio-block &optional))
(defun encode-block (streaminfo channels)
  "Encode channels and return wavelet audio block ready for
writing. NB: content of @c(channels) is destructively modified."
  (declare (optimize (speed 3)))
  (let ((skip-steps (streaminfo-skip-steps streaminfo))
        (block-size (streaminfo-block-size streaminfo)))
    (declare (type skip-steps skip-steps))
    (if (= (length channels) 2)
        (apply #'decorrelate-channels channels))
    (flet ((encode-channel (channel)
             (dwt! (add-padding channel block-size)
                   :wavelet :cdf-4-2
                   :steps (- skip-steps))))
      (make-instance 'wavelet-audio-block
                     :channels (mapcar #'encode-channel channels)
                     :streaminfo streaminfo))))

(sera:-> write-block (bit-output-stream wavelet-audio-block)
         (values wavelet-audio-block &optional))
(defun write-block (stream wa-block)
  "Write encoded block @cl:param(wa-block) to the stream
@cl:param(stream)."
  (declare (optimize (speed 3)))
  ;; Write sync code
  (write-octet #xff stream)
  (write-octet #xfe stream)
  ;; Write block number
  (write-block-number stream (block-number wa-block))

  (let* ((streaminfo (block-streaminfo wa-block))
         (skip-steps (streaminfo-skip-steps streaminfo))
         (block-size (streaminfo-block-size streaminfo))
         (history-size (streaminfo-history-size streaminfo)))
    (declare (type skip-steps skip-steps)
             (type (ub 8) history-size)
             (type (ub 16) block-size))
    (dolist (channel (block-channels wa-block))
      (declare (type (simple-array (sb 32)) channel))
      (loop with start-idx = 0
            for i from skip-steps below (integer-length block-size)
            for end-idx = (ash 1 i)
            for history = (make-history history-size) do
            (loop for idx from start-idx below end-idx do
                  (adaptive-write history stream (aref channel idx)))
            (setq start-idx end-idx))))
  (pad-to-byte-alignment 0 stream)
  wa-block)

(defun encode-wavelet-audio (input-name output-name)
  "Convert uncompressed wav file with name @cl:param(input-name) to
wavelet-audio file with name @cl:param(output-name)."
  (with-open-file (input input-name :element-type '(unsigned-byte 8))
    (let* ((reader (wav:open-wav input))
           (header (wav:read-wav-header reader))
           (samples-num (wav:samples-num header))
           (streaminfo (make-instance 'wavelet-audio-streaminfo
                                      :samplerate (wav:format-samplerate (car header))
                                      :channels (wav:format-channels-num (car header))
                                      :bps (wav:format-bps (car header))
                                      :samples samples-num)))
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
          (loop for block-start below samples-num by *block-size*
                for data-read from 0 by *block-size*
                for block-count from 0 by 1
                for data = (wav:read-wav-data reader (car header)
                                              (min *block-size* (- samples-num data-read))
                                              :decompose t)
                for wa-block = (encode-block streaminfo data)
                do
                (setf (block-number wa-block) block-count)
                (write-block s wa-block))
          (flush-bit-output-stream s)))))
  (values))

;; Decoding

(sera:-> correlate-channels ((sa-sb 32) (sa-sb 32))
         (values (sa-sb 32) (sa-sb 32) &optional))
(defun correlate-channels (chan1 chan2)
  "Inverse DECORRELATE-CHANNELS"
  (declare (optimize (speed 3)))
  (map-into
   chan1 (lambda (x y)
           (- x (truncate y 2)))
   chan1 chan2)
  (map-into chan2 #'+ chan2 chan1)
  (values chan1 chan2))

(declaim (inline check-wavelet-audio))
(defun check-wavelet-audio (stream)
  (unless (and (= (read-octet stream) #x57)
               (= (read-octet stream) #x61)
               (= (read-octet stream) #x56)
               (= (read-octet stream) #x65))
    (error 'wavelet-audio-error :format-control "Not a wavelet audio file")))

(sera:-> read-block (bit-input-stream wavelet-audio-streaminfo)
         (values wavelet-audio-block &optional))
(defun read-block (stream streaminfo)
  "Read block from previously opened stream
  @cl:param(stream). @cl:param(streaminfo) metadata block must be
  given."
  (declare (optimize (speed 3)))
  (unless (and (= (read-octet stream) #xff)
               (= (read-octet stream) #xfe))
    (error 'wavelet-audio-frame-error :stream stream))
  (let* ((channels (streaminfo-channels streaminfo))
         (block-size (streaminfo-block-size streaminfo))
         (skip-steps (streaminfo-skip-steps streaminfo))
         (history-size (streaminfo-history-size streaminfo))
         (wa-block (make-instance 'wavelet-audio-block
                                  :streaminfo streaminfo
                                  :channels (loop repeat channels collect
                                                 (make-array block-size
                                                             :element-type '(signed-byte 32)))
                                  :number (read-block-number stream))))
    (declare (type (ub 8) channels)
             (type (ub 16) block-size)
             (type skip-steps skip-steps))
    (dolist (channel (block-channels wa-block))
      (declare (type (simple-array (sb 32)) channel))
      (loop with start-idx = 0
            for i from skip-steps below (integer-length block-size)
            for end-idx = (ash 1 i)
            for history = (make-history history-size) do
            (loop for idx from start-idx below end-idx do
                  (setf (aref channel idx) (adaptive-read history stream)))
            (setq start-idx end-idx)))
    (read-to-byte-alignment stream)
    wa-block))

(sera:-> decode-block (wavelet-audio-block)
         (values list &optional))
(defun decode-block (wa-block)
  "Decode audio block."
  (declare (optimize (speed 3)))
  (let ((skip-steps (streaminfo-skip-steps
                     (block-streaminfo wa-block))))
    (declare (type skip-steps skip-steps))
    (flet ((decode-channel (channel)
             (dwt-inverse! channel
                           :wavelet :cdf-4-2
                           :steps (- skip-steps))))
      (let ((decoded-channels (mapcar #'decode-channel (block-channels wa-block))))
        (if (= (length decoded-channels) 2)
            (apply #'correlate-channels decoded-channels))
        decoded-channels))))

(sera:-> open-wavelet-audio (bit-input-stream)
         (values list &optional))
(defun open-wavelet-audio (stream)
  "Check if @cl:param(stream) is wavelet audio stream and read metadata."
  (check-wavelet-audio stream)
  (let* ((metadata (read-metadata stream))
         (streaminfo (first metadata)))
    (unless (typep streaminfo 'wavelet-audio-streaminfo)
      (error 'wavelet-audio-error :format-control "No streaminfo in the stream"))
    (when (> (streaminfo-version streaminfo) +max-version+)
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
        (wav:with-output-to-wav (output output-name
                                        :supersede t
                                        :samplerate samplerate
                                        :channels channels
                                        :bps bps
                                        :totalsamples samples)
          (loop for samples-left downfrom samples to 1 by block-size
                for samples-in-block = (min block-size samples-left)
                for decoded-bufs = (decode-block (read-block s streaminfo)) do
                (write-sequence (core:interleave-channels decoded-bufs)
                                output :end (* samples-in-block channels)))))))
  (values))
