(defsystem :wavelet-audio
  :name :wavelet-audio
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Proof of concept for lossless audio compressor"
  :components ((:file "src/packages")
               (:file "src/wavelet" :depends-on ("src/packages"))
               (:file "src/definitions" :depends-on ("src/packages"))
               (:file "src/history" :depends-on ("src/packages"))
               (:file "src/io" :depends-on ("src/packages"))
               (:file "src/metadata" :depends-on ("src/packages"))
               (:file "src/wavelet-audio" :depends-on ("src/packages"))
               (:file "src/seek" :depends-on ("src/packages")))
  :depends-on (:easy-audio :trivial-bit-streams))
