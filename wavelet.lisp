(in-package :wavelet-transform)

;; This code uses (4,2) B-spline wavelet with factorization given in
;; "Factoring wavelet transforms into lifting steps" (Daubechies 1996)

(declaim (ftype (function (fixnum) fixnum) check-array-size))
(defun check-array-size (size)
  "Ensure array size is power-of-two"
  (if (zerop (logand size (1- size)))
      size
      (error "Array size is not a power of two")))

(defmacro with-gensym (symbols &body body)
  `(let ,(loop for sym in symbols collect
              `(,sym (gensym)))
     ,@body))

(defmacro def-lifting-scheme (name &body clauses)
  "Define lifting scheme step"
  (with-gensym (skip start end i idx2 array)
    `(defun ,name (,array ,skip ,start ,end)
       (declare (optimize (speed 3))
                (type (simple-array (signed-byte 32)) ,array)
                (type fixnum ,skip ,start ,end))
       ,@(loop for clause in clauses collect
              `(loop for ,i fixnum from ,start below ,end by ,skip do
                    (symbol-macrolet
                        ,(loop for vardef in (car clause) collect
                              (destructuring-bind (varname type idx &optional fallback) vardef
                                (declare (type (member :odd :even) type)
                                         (type integer idx))
                                (let ((idx2% `(the fixnum
                                                   (+ ,i ,(if (eq type :even) '0 `(ash ,skip -1))
                                                      (* ,idx ,skip)))))
                                  `(,varname ,(cond
                                                ((< idx 0) `(let ((,idx2 ,idx2%))
                                                              (if (< ,idx2 ,start) ,fallback (aref ,array ,idx2))))
                                                ((> idx 0) `(let ((,idx2 ,idx2%))
                                                              (if (< ,idx2 ,end) (aref ,array ,idx2) ,fallback)))
                                                (t `(aref ,array ,idx2%)))))))
                      ,@(cdr clause))))
       ,array)))

(def-lifting-scheme wavelet-forward-step
  (((odd :odd 0)
    (even :even 0)
    (odd-1 :odd -1 odd))
   (decf even (truncate (+ odd odd-1) 4)))

  (((even :even 0)
    (odd :odd 0)
    (even+1 :even 1 even))
   (decf odd (+ even even+1)))

  (((odd :odd 0)
    (even :even 0)
    (odd-1 :odd -1 odd))
   (decf even (- (truncate odd 16) (truncate (* 3 odd-1) 16))))
  
  (((odd :odd 0)
    (even :even 0))
   (incf odd (* 2 even))
   (incf even (truncate odd 2))
   (decf odd even)))

(def-lifting-scheme wavelet-inverse-step
  (((odd :odd 0)
    (even :even 0))
   (incf odd even)
   (decf even (truncate odd 2))
   (decf odd (* 2 even)))

  (((odd :odd 0)
    (even :even 0)
    (odd-1 :odd -1 odd))
   (incf even (- (truncate odd 16) (truncate (* 3 odd-1) 16))))

  (((even :even 0)
    (odd :odd 0)
    (even+1 :even 1 even))
   (incf odd (+ even even+1)))
  
  (((odd :odd 0)
    (even :even 0)
    (odd-1 :odd -1 odd))
   (incf even (truncate (+ odd odd-1) 4))))

(defun transform (array function &key (start 0) end inverse)
  "Generic transform function"
  (declare (optimize (speed 3))
           (type fixnum start)
           (type (or null fixnum) end)
           (type function function)
           (type (simple-array (signed-byte 32)) array))
  (let* ((end (if end end (length array)))
         (length (- end start))
         (steps (integer-length (check-array-size length))))
    (loop for i from 1 below steps do
         (setq array (funcall function array (ash 1 (if inverse (- steps i) i))
                              start end))))
  array)

(defun wavelet-forward (array &key (start 0) end)
  "Perform in-place forward wavelet transform in ARRAY of type (SIMPLE-ARRAY
  (SIGNED-BYTE 32)). START and END can be used for definition of transform bounds"
  (transform array #'wavelet-forward-step
             :start start
             :end end))

(defun wavelet-inverse (array &key (start 0) end)
  "Perform in-place inverse wavelet transform in ARRAY of type (SIMPLE-ARRAY
  (SIGNED-BYTE 32)). START and END can be used for definition of transform bounds"
  (transform array #'wavelet-inverse-step
             :start start
             :end end
             :inverse t))

(defun forward-recopy (array)
  (declare (optimize (speed 3))
           (type (simple-array (signed-byte 32)) array))
  (let* ((len (length array))
         (new-array (make-array len
                                :element-type '(signed-byte 32)))
         (steps (1- (integer-length len))))

    (setf (aref new-array 0)
          (aref array 0))
    (loop for i below steps do
         (loop
            for input-idx from (ash 1 i) by (ash 1 (1+ i)) below len
            for out-idx from (ash len (- (1+ i))) by 1 do
              (setf (aref new-array out-idx)
                    (aref array input-idx))))
    new-array))

(defun inverse-recopy (array)
  (declare (optimize (speed 3))
           (type (simple-array (signed-byte 32)) array))
  (let* ((len (length array))
         (new-array (make-array (length array)
                                :element-type '(signed-byte 32)))
         (steps (1- (integer-length len))))

    (setf (aref new-array 0)
          (aref array 0))
    (loop for i below steps do
         (loop
            for input-idx from (ash len (- (1+ i)))
            for out-idx from (ash 1 i) by (ash 1 (1+ i)) below len do
              (setf (aref new-array out-idx)
                    (aref array input-idx))))
    new-array))

(defun wavelet-forward-w/recopy (array)
  "Perform out-of place forward wavelet transform. The result is recopied to
have larger scale coefficients in the beginning of array and smaller scale 
coefficients in the end."
  (forward-recopy
   (wavelet-forward array)))

(defun wavelet-inverse-w/recopy (array)
  "Perform out-of place inverse wavelet transform returned by WAVELET-FORWARD-W/RECOPY."
  (wavelet-inverse
   (inverse-recopy array)))
