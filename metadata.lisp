(in-package :wavelet-audio)

(defmethod write-metadata-header (stream (metadata wavelet-audio-metadata))
  (write-bits (metadata-type metadata) 7 stream)
  (write-bit (if (metadata-last-p metadata) 1 0) stream)
  (write-bits (metadata-size metadata) 32 stream)
  metadata)

(defmethod read-metadata-header (stream (metadata wavelet-audio-metadata))
  (setf (metadata-type metadata)
        (read-bits 7 stream)
        (metadata-last-p metadata)
        (not (zerop (read-bit stream)))
        (metadata-size metadata)
        (read-bits 32 stream))
  metadata)

(defmethod write-metadata-body (stream (streaminfo wavelet-audio-streaminfo))
  (write-octet (streaminfo-version streaminfo) stream)
  (write-bits (streaminfo-samplerate streaminfo) 24 stream)
  (write-octet (streaminfo-bps streaminfo) stream)
  (write-octet (streaminfo-channels streaminfo) stream)
  (write-bits (streaminfo-samples streaminfo) 32 stream)
  (write-bits (streaminfo-block-size streaminfo) 16 stream)
  streaminfo)

(defun write-metadata (stream list)
  (mapc (lambda (metadata)
          (setf (metadata-last-p metadata) nil))
        (butlast list))
  (setf (metadata-last-p (car (last list))) t)
  (mapc (lambda (metadata)
          (write-metadata-header stream metadata)
          (write-metadata-body stream metadata))
        list))

(defmethod read-metadata-body (stream (metadata wavelet-audio-metadata))
  (let ((len (metadata-size metadata)))
    (read-octet-vector (make-array len :element-type '(ub 8)) stream))
  metadata)

(defmethod read-metadata-body (stream (streaminfo wavelet-audio-streaminfo))
  (setf (streaminfo-version streaminfo) (read-octet stream)
        (streaminfo-samplerate streaminfo) (read-bits 24 stream)
        (streaminfo-bps streaminfo) (read-octet stream)
        (streaminfo-channels streaminfo) (read-octet stream)
        (streaminfo-samples streaminfo) (read-bits 32 stream)
        (streaminfo-block-size streaminfo) (read-bits 16 stream))
  streaminfo)

(defun transform-metadata (metadata)
  (change-class metadata
                (cond
                  ((= (metadata-type metadata) +metadata-streaminfo+)
                   'wavelet-audio-streaminfo)
                  (t 'wavelet-audio-metadata))))

(defun read-metadata (stream)
  (loop
     for metadata = (make-instance 'wavelet-audio-metadata)
     collect
       (read-metadata-body
        stream
        (transform-metadata (read-metadata-header stream metadata)))
     until (metadata-last-p metadata)))
