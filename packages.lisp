(defpackage wavelet-transform
  (:use #:cl)
  (:export #:wavelet-forward
           #:wavelet-inverse
           #:wavelet-forward-w/recopy
           #:wavelet-inverse-w/recopy))

(defpackage wavelet-audio
  (:use #:cl #:wavelet-transform)
  (:export #:encode-wavelet-audio
           #:decode-wavelet-audio
           #:play-wavelet-audio

           #:wavelet-audio-error
           #:wavelet-audio-broken-header))
