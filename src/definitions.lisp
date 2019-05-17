(in-package :wavelet-audio)

(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(deftype ub (n) (list 'unsigned-byte n))
(deftype sb (n) (list 'signed-byte n))
(deftype rice-parameter () '(integer 0 32))

(defconstant +metadata-streaminfo+ 0)

(defclass wavelet-audio-metadata ()
  ((type  :type (ub 7)
          :initarg :type
          :accessor metadata-type
          :documentation "Type of metadata.")
   (lastp :type boolean
          :initarg :lastp
          :accessor metadata-last-p
          :documentation "@c(T) if this metadata block is the last in the stream.")
   (size  :type non-negative-fixnum
          :initarg :size
          :accessor metadata-size
          :documentation "Size of this metadata block in bytes."))
  (:documentation "Generic metadata class"))

(defclass wavelet-audio-streaminfo (wavelet-audio-metadata)
  ((version      :type (ub 8)
                 :accessor streaminfo-version
                 :initarg :version
                 :documentation "Stream version.")
   (samplerate   :type (ub 24)
                 :accessor streaminfo-samplerate
                 :initarg :samplerate
                 :documentation "Sample rate in Hertz.")
   (bps          :type (ub 8)
                 :accessor streaminfo-bps
                 :initarg :bps
                 :documentation "Bits per sample.")
   (channels     :type (ub 8)
                 :accessor streaminfo-channels
                 :initarg :channels
                 :documentation "Number of channels.")
   (samples      :type (ub 32)
                 :accessor streaminfo-samples
                 :initarg :samples
                 :documentation "Number of interchannel samples.")
   (block-size   :type (ub 16)
                 :accessor streaminfo-block-size
                 :initarg :block-size
                 :documentation "Block size in samples.")
   (history-size :type (ub 8)
                 :accessor streaminfo-history-size
                 :initarg :history-size
                 :documentation "History size for adaptive Rice coder"))
  (:default-initargs
   :size 13
   :type +metadata-streaminfo+)
  (:documentation "Stream info metadata."))

(defgeneric write-metadata-header (stream metadata))
(defgeneric write-metadata-body (stream metadata))

(defgeneric read-metadata-header (stream metadata))
(defgeneric read-metadata-body (stream metadata))

(defclass wavelet-audio-block ()
  ((streaminfo :type wavelet-audio-streaminfo
               :accessor block-streaminfo
               :initarg :streaminfo
               :documentation "streaminfo metadata block for this stream")
   (channels   :type list
               :accessor block-channels
               :initarg :channels
               :documentation "Channel buffers.")
   (number     :type non-negative-fixnum
               :accessor block-number
               :initarg :number
               :documentation "Number of block in the stream."))
  (:documentation "Audio block."))

(define-condition wavelet-audio-condition (simple-condition)
  ()
  (:documentation "Generic wavelet-audio condition"))

(define-condition wavelet-audio-error (wavelet-audio-condition error)
  ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Wavelet-audio error: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "wavelet-audio error"))

(define-condition wavelet-audio-warning (wavelet-audio-condition warning)
  ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Wavelet-audio warning: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "wavelet-audio warning"))

(define-condition wavelet-audio-frame-error (wavelet-audio-error)
  ((stream :initarg :stream
           :reader frame-error-stream))
  (:default-initargs
   :format-control "Frame error")
  (:documentation "wavelet-audio frame error, e.g. stream is out of sync"))

(define-condition wavelet-audio-unknown-metadata (wavelet-audio-warning)
  ((metadata :initarg :metadata
             :reader unknown-metadata))
  (:documentation "Signaled when metadata type is unknown"))
