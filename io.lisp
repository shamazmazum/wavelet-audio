(in-package :wavelet-audio)
(declaim (optimize (speed 3)))

;; Writing
;; Rice coding functions
(defun write-rice-unsigned (stream residual m)
  "Write unsigned value @cl:param(residual) into stream @c(stream)
using Rice coding with parameter @cl:param(m). If @cl:param(stream) is
@c(nil), only internal bit counter is updated."
  (declare (type rice-parameter m)
           (type (unsigned-byte 32) residual))
  (let ((quotient (ash residual (- m)))
        (remainder (logand residual (1- (ash 1 m)))))
    (when stream
      (loop repeat quotient do (write-bit 1 stream))
      (write-bit 0 stream)
      (write-bits remainder m stream))))

(defun write-rice (stream residual m)
  "Write signed value @cl:param(residual) into stream
@cl:param(stream) using Rice coding with parameter @cl:param(m). If
@cl:param(stream) is @c(nil), only internal bit counter is updated."
  (declare (type (signed-byte 32) residual))
  (write-rice-unsigned
   stream
   (+ (* 2 (abs residual))
      (if (< residual 0) 1 0))
   m))

(defstruct adaptive-coder
  (count 0 :type unsigned-byte)
  (sum 0 :type unsigned-byte))

(defun adaptive-write (coder stream residual)
  (with-accessors ((count adaptive-coder-count)
                   (sum adaptive-coder-sum))
      coder
    (let* ((avg (if (zerop count)
                    (abs residual)
                    (truncate sum count)))
           (p (integer-length avg)))
      (declare (type rice-parameter p))
      (if (zerop count)
          (write-bits p 5 stream))
      (write-rice stream residual p))
    (incf sum (abs residual))
    (incf count)))

(defun adaptive-read (coder stream)
  (with-accessors ((count adaptive-coder-count)
                   (sum adaptive-coder-sum))
      coder
    (let ((residual
           (read-rice
            stream
            (the rice-parameter
                 (if (zerop count)
                     (read-bits 5 stream)
                     (integer-length (truncate sum count)))))))
      (incf count)
      (incf sum (abs residual))
      residual)))

(defun write-block-number (stream block-number)
  "Compactly code block number @cl:param(block-number) into stream
@cl:param(stream)."
  (declare (type non-negative-fixnum block-number))
  (let ((bits (logand #x7f block-number)))
    (cond
      ((/= block-number bits)
       (write-octet (logior #x80 bits) stream)
       (write-block-number stream (ash block-number -7)))
      (t
       (write-octet bits stream)))))

;; Reading
(declaim (ftype (function (t rice-parameter)
                          (ub 32))
                read-rice-unsigned))
(defun read-rice-unsigned (stream m)
  "Read unsigned Rice coded value from @cl:param(stream). @cl:param(m)
is the Rice code parameter."
  (declare (type rice-parameter m))
  (let ((quotient
         (loop
            for bit = (read-bit stream)
            until (zerop bit)
            sum 1 fixnum))
        (remainder (read-bits m stream)))
    (declare (type (ub 32) quotient remainder))
    (+ remainder (ash quotient m))))

(defun read-rice (stream m)
    "Read signed Rice coded value from @cl:param(stream). @cl:param(m)
is the Rice code parameter."
  (let* ((unsigned (read-rice-unsigned stream m))
         (res (ash unsigned -1)))
    (declare (type (ub 32) unsigned res))
    (if (zerop (logand unsigned 1))
        res (- res))))

(defun read-block-number (stream)
  "Read block number from stream."
  (labels ((read-block-number% (result octets)
             (declare (type non-negative-fixnum result)
                      (type (integer 0 32) octets))
             (let* ((octet (read-octet stream))
                    (result% (logior result (ash (logand #x7f octet) octets))))
               (if (zerop (logand #x80 octet)) result%
                   (read-block-number% result% (+ octets 7))))))
    (declare (dynamic-extent #'read-block-number%))
    (read-block-number% 0 0)))
