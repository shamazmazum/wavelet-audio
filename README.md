Wavelet-audio
-------------

This package is a proof of concept of wavelet transform-based lossless audio
codec. **wavelet-audio** uses  (4,2) B-spline wavelet and Rice entropy coding to compress
audio. Compression speed is slow, decompression speed is much faster, but is limited by bad
performance of `trivial-bit-streams`.

Also, because of limitations of `trivial-bit-streams`, underlying audio format does not support
seeking and the whole stream must be decoded first to get to its ending.

Compression ratio is 25-50% and is comparable to ratio of flac codec.

Examples:
========

~~~~~~~~{lisp}
(wavelet-audio:encode-wavelet-audio "~/file.wav" "~/file.wa")
(wavelet-audio:decode-wavelet-audio "~/file.wa" "~/file.wav")
~~~~~~~~
