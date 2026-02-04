(defun do-all()
  (handler-case
      (ql:quickload :wavelet-audio/tests)
    (asdf:compile-file-error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "wavelet-audio-tests:run-tests")
       0 1)))

(do-all)
