(defsystem :wavelet-audio
  :name :wavelet-audio
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Proof of concept for lossless audio compressor"
  :components ((:file "packages")
               (:file "wavelet" :depends-on ("packages"))
               (:file "definitions" :depends-on ("packages"))
               (:file "io" :depends-on ("packages"))
               (:file "metadata" :depends-on ("packages"))
               (:file "wavelet-audio" :depends-on ("packages")))
  :depends-on (:easy-audio :trivial-bit-streams))
