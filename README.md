Wavelet-audio
-------------

This package is a proof of concept of wavelet transform-based lossless audio
codec. **wavelet-audio** uses  (4,2) B-spline wavelet and Rice entropy coding to compress
audio. Compression speed is slow, decompression speed is much faster.

Also, because of limitations of `trivial-bit-streams`, the decoder does not support seeking seeking
and the whole stream must be decoded first to get to its ending. Support for per-block seeking is
present in the format design, although.

Compression ratio is 25-50% and is comparable to ratio of flac codec.

Examples:
========

~~~~~~~~{lisp}
(wavelet-audio:encode-wavelet-audio "~/file.wav" "~/file.wa")
(wavelet-audio:decode-wavelet-audio "~/file.wa" "~/file.wav")
~~~~~~~~
