(in-package :wavelet-audio)

;; Writing
(sera:-> write-rice-unsigned (bit-output-stream (ub 32) rice-parameter)
         (values (ub 32) &optional))
(defun write-rice-unsigned (stream residual m)
  "Write unsigned value @cl:param(residual) into stream @c(stream)
using Rice coding with parameter @cl:param(m). If @cl:param(stream) is
@c(nil), only internal bit counter is updated."
  (declare (optimize (speed 3)))
  (let ((quotient (ash residual (- m)))
        (remainder (logand residual (1- (ash 1 m)))))
    (loop repeat quotient do (write-bit 1 stream))
    (write-bit 0 stream)
    (write-bits remainder m stream)))

(sera:-> write-rice (bit-output-stream (sb 32) rice-parameter)
         (values (sb 32) &optional))
(defun write-rice (stream residual m)
  "Write signed value @cl:param(residual) into stream
@cl:param(stream) using Rice coding with parameter @cl:param(m). If
@cl:param(stream) is @c(nil), only internal bit counter is updated."
  (declare (optimize (speed 3)))
  (write-rice-unsigned
   stream
   (+ (* 2 (abs residual))
      (if (< residual 0) 1 0))
   m))

(sera:-> write-block-number (bit-output-stream non-negative-fixnum)
         (values non-negative-fixnum &optional))
(defun write-block-number (stream block-number)
  "Compactly code block number @cl:param(block-number) into stream
@cl:param(stream)."
  (declare (optimize (speed 3)))
  (labels ((%go (block-number)
             (let ((bits (logand #x7f block-number)))
               (cond
                 ((/= block-number bits)
                  (write-octet (logior #x80 bits) stream)
                  (%go (ash block-number -7)))
                 (t
                  (write-octet bits stream))))))
    (%go block-number)
    block-number))

;; Reading
(sera:-> read-rice-unsigned (bit-input-stream rice-parameter)
         (values (ub 32) &optional))
(defun read-rice-unsigned (stream m)
  "Read unsigned Rice coded value from @cl:param(stream). @cl:param(m)
is the Rice code parameter."
  (declare (optimize (speed 3)))
  (let ((quotient
         (loop for bit = (read-bit stream)
               until (zerop bit)
               sum 1 of-type (ub 32)))
        (remainder (read-bits m stream)))
    (+ remainder (ash quotient m))))

(sera:-> read-rice (bit-input-stream rice-parameter)
         (values (sb 32) &optional))
(defun read-rice (stream m)
  "Read signed Rice coded value from @cl:param(stream). @cl:param(m)
is the Rice code parameter."
  (declare (optimize (speed 3)))
  (let* ((unsigned (read-rice-unsigned stream m))
         (res (floor unsigned 2)))
    (if (evenp unsigned)
        res (- res))))

(sera:-> read-block-number (bit-input-stream)
         (values non-negative-fixnum &optional))
(defun read-block-number (stream)
  "Read block number from stream."
  (declare (optimize (speed 3)))
  (labels ((%read-block-number (result octets)
             (declare (type non-negative-fixnum result)
                      (type (integer 0 32) octets))
             (let* ((octet (read-octet stream))
                    (%result (logior result (ash (logand #x7f octet) octets))))
               (if (zerop (logand #x80 octet)) %result
                   (%read-block-number %result (+ octets 7))))))
    (%read-block-number 0 0)))

(sera:-> adaptive-write (history bit-output-stream (sb 32))
         (values (sb 32) &optional))
(defun adaptive-write (history stream residual)
  (declare (optimize (speed 3)))
  (let ((p (integer-length
            (if (zerop (history-count history))
                (abs residual)
                (history-avg history)))))
    (if (zerop (history-count history))
        (write-bits p 5 stream))
    (write-rice stream residual p))
  (history-insert history (abs residual))
  residual)

(sera:-> adaptive-read (history bit-input-stream)
         (values (sb 32) &optional))
(defun adaptive-read (history stream)
  (declare (optimize (speed 3)))
  (let ((residual
         (read-rice
          stream (if (zerop (history-count history))
                     (read-bits 5 stream)
                     (integer-length (history-avg history))))))
    (history-insert history (abs residual))
    residual))
