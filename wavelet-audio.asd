(defsystem :wavelet-audio
  :name :wavelet-audio
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Proof of concept for lossless audio compressor"
  :components ((:file "src/package")
               (:file "src/definitions" :depends-on ("src/package"))
               (:file "src/history" :depends-on ("src/package"))
               (:file "src/io" :depends-on ("src/package"))
               (:file "src/metadata" :depends-on ("src/package"))
               (:file "src/wavelet-audio" :depends-on ("src/package"))
               (:file "src/seek" :depends-on ("src/package")))
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
  :description "Proof of concept for lossless audio compressor"
  :components ((:file "tests/package")
               (:file "tests/tests" :depends-on ("tests/package")))
  :depends-on (:wavelet-audio :fiveam :flexi-streams))
