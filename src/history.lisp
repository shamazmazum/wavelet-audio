(in-package :wavelet-audio)

(defstruct (history
             (:constructor %make-history))
  (array (make-array 0 :element-type '(ub 32))
           :type (simple-array (ub 32) (*)))
  (pos   0 :type non-negative-fixnum)
  (count 0 :type non-negative-fixnum)
  (sum   0 :type non-negative-fixnum))

(sera:-> make-history (positive-fixnum)
         (values history &optional))
(defun make-history (size)
  (%make-history
   :array (make-array size :element-type '(ub 32))))

(sera:-> history-insert (history (ub 32))
         (values (ub 32) &optional))
(defun history-insert (history value)
  (declare (optimize (speed 3)))
  (with-accessors ((pos history-pos)
                   (count history-count)
                   (array history-array)
                   (sum history-sum))
      history
    (let ((size (length array)))
      (setf count (min (1+ count) size)
            pos (rem (1+ pos) size)
            sum (+ sum value
                   (if (= size count)
                       (- (aref array pos)) 0))
            (aref array pos) value)))
  value)

(sera:-> history-avg (history)
         (values (ub 32) &optional))
(defun history-avg (history)
  (declare (optimize (speed 3)))
  (nth-value
   0 (truncate (history-sum history)
               (history-count history))))
