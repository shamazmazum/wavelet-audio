(defun do-all()
  (asdf:load-system :wavelet-audio-tests)
  (sb-ext:exit
   :code
   (if (funcall
        (intern (symbol-name :run-tests)
                (find-package :wavelet-audio-tests)))
        0 1)))

(do-all)
