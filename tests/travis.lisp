(defun do-all()
  (handler-case
      (asdf:load-system :wavelet-audio/tests)
    (asdf:compile-file-error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "wavelet-audio-tests:run-tests")
       0 1)))

(do-all)
