(in-package :wavelet-audio)

(declaim (type non-negative-fixnum *count*))
(defvar *count* 0)
(declaim (optimize (speed 3)))

;; Writing
;; Rice coding functions
(defun write-rice-unsigned (stream residual m)
  (declare (type rice-parameter m)
           (type (unsigned-byte 32) residual))
  (let ((quotient (ash residual (- m)))
        (remainder (logand residual (1- (ash 1 m)))))
    (incf *count* (+ quotient 1 m))
    (when stream
      (loop repeat quotient do (write-bit 1 stream))
      (write-bit 0 stream)
      (write-bits remainder m stream))))

(defun write-rice (stream residual m)
  (declare (type (signed-byte 32) residual))
  (write-rice-unsigned
   stream
   (+ (* 2 (abs residual))
      (if (< residual 0) 1 0))
   m))

(defun write-block-number (stream block-number)
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
  (let* ((unsigned (read-rice-unsigned stream m))
         (res (ash unsigned -1)))
    (declare (type (ub 32) unsigned res))
    (if (zerop (logand unsigned 1))
        res (- res))))

(defun read-block-number (stream)
  (labels ((read-block-number% (result octets)
             (declare (type non-negative-fixnum result)
                      (type (integer 0 32) octets))
             (let* ((octet (read-octet stream))
                    (result% (logior result (ash (logand #x7f octet) octets))))
               (if (zerop (logand #x80 octet)) result%
                   (read-block-number% result% (+ octets 7))))))
    (declare (dynamic-extent #'read-block-number%))
    (read-block-number% 0 0)))
