(in-package :wavelet-audio)
(declaim (optimize (speed 3)))

(defstruct (history
             (:constructor make-history%))
  array
  (pos 0 :type non-negative-fixnum)
  (count 0 :type non-negative-fixnum)
  (sum 0 :type non-negative-fixnum))

(defun make-history (size)
  (make-history%
   :array (make-array size :element-type '(ub 32))))

(defun history-insert (history value)
  (declare (type (ub 32) value)
           (type history history))
  (with-accessors ((pos history-pos)
                   (count history-count)
                   (array history-array)
                   (sum history-sum))
      history
    (declare (type (simple-array (ub 32)) array))
    (let ((size (length array)))
      (setf count (min (1+ count) size)
            pos (rem (1+ pos) size)
            sum (+ sum value
                   (if (= size count)
                       (- (aref array pos)) 0))
            (aref array pos) value)))
  value)

(declaim (ftype (function (history &optional) (ub 32)) history-avg))
(defun history-avg (history)
  (declare (type history history))
  (truncate (history-sum history)
            (history-count history)))
