(defsystem :wavelet-audio-tests
  :name :wavelet-audio-tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Proof of concept for lossless audio compressor"
  :components ((:file "tests/package")
               (:file "tests/tests" :depends-on ("tests/package")))
  :depends-on (:wavelet-audio :fiveam :flexi-streams))
