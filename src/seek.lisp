(in-package :wavelet-audio)

(defparameter *buffer-len* 4096)
(defun peek-octet (stream octet)
  (let* ((buffer (make-array *buffer-len* :element-type '(unsigned-byte 8)))
         (end (read-sequence buffer stream))
         (pos (position octet buffer :end end)))
    (cond
      (pos
       (file-position stream
                      (+ (file-position stream)
                         pos
                         (- *buffer-len*)))
       octet)
      (t
       (when (zerop end) (return-from peek-octet nil))
       (peek-octet stream octet)))))

(defun restore-sync (stream streaminfo)
  (peek-octet stream #xff)
  (let ((pos (file-position stream)))
    (handler-case
        (with-bit-input-stream (bit-stream :callback (make-stream-input-callback stream))
          (prog1
              (block-number (read-block bit-stream streaminfo))
            (file-position stream pos)))
      ((or type-error wavelet-audio-frame-error) ()
        (file-position stream (1+ pos))
        (restore-sync stream streaminfo)))))

(defun seek-sample (stream streaminfo sample-num)
  (if (> sample-num (streaminfo-samples streaminfo))
      (error 'wavelet-audio-error :format-control "sample-num exceeds maximum value"))
  (multiple-value-bind (block-num remainder)
      (floor sample-num (streaminfo-block-size streaminfo))
    (labels ((seek% (start end)
               (let* ((mid (floor (+ end start) 2))
                      (cur-block-num (progn (file-position stream mid)
                                            (restore-sync stream streaminfo))))
                 (cond
                   ((< cur-block-num block-num)
                    (seek% mid end))
                   ((> cur-block-num block-num)
                    (seek% start mid))
                   (t (values block-num remainder))))))
      (seek% 0 (file-length stream)))))
