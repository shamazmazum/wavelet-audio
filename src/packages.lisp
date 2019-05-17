(defpackage wavelet-transform
  (:use #:cl)
  (:export #:wavelet-forward
           #:wavelet-inverse
           #:wavelet-forward-w/recopy
           #:wavelet-inverse-w/recopy))

(defpackage wavelet-audio
  (:use #:cl #:wavelet-transform #:trivial-bit-streams)
  (:export #:encode-wavelet-audio
           #:decode-wavelet-audio
           #:open-wavelet-audio
           #:write-wavelet-audio-header

           #:wavelet-audio-block
           #:encode-block
           #:write-block
           #:decode-block
           #:read-block

           #:wavelet-audio-streaminfo
           #:streaminfo-version
           #:streaminfo-samplerate
           #:streaminfo-bps
           #:streaminfo-channels
           #:streaminfo-samples
           #:streaminfo-block-size

           #:wavelet-audio-error
           #:wavelet-audio-warning
           #:wavelet-audio-frame-error
           #:wavelet-audio-unknown-metadata

           #:*block-size*
           #:*min-rice-parameter*
           #:*max-rice-parameter*))
