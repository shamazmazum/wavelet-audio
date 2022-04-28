(defsystem :wavelet-audio
  :name :wavelet-audio
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Proof of concept for lossless audio compressor"
  :licence "2-clause BSD"
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "definitions")
               (:file "history")
               (:file "io")
               (:file "metadata")
               (:file "wavelet-audio")
               (:file "seek"))
  :depends-on (:easy-audio :trivial-bit-streams :cl-wavelets)
  :in-order-to ((test-op (load-op "wavelet-audio/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :wavelet-audio-tests))))))

(defsystem :wavelet-audio/tests
  :name :wavelet-audio/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:wavelet-audio :fiveam :flexi-streams))
