(defpackage wavelet-audio
  (:use #:cl #:cl-wavelets #:trivial-bit-streams)
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

           #:restore-sync
           #:seek-sample

           #:*block-size*
           #:*history-size*
           #:*skip-steps*))
