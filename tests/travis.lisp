(defun do-all()
  (ql:quickload :wavelet-audio/tests)
  (uiop:quit
   (if (uiop:call-function "wavelet-audio-tests:run-tests")
       0 1)))

(do-all)
