Wavelet-audio
-------------
[![Build Status](https://api.cirrus-ci.com/github/shamazmazum/wavelet-audio.svg)](https://cirrus-ci.com/github/shamazmazum/wavelet-audio)
![CI](https://github.com/shamazmazum/wavelet-audio/workflows/CI/badge.svg)

This package is a proof of concept of wavelet transform-based lossless
audio codec. **wavelet-audio** uses  (4,2) B-spline wavelet and Rice
entropy coding to compress audio. Compression speed is slow,
decompression speed is much faster.

Compression ratio is 25-50% and is comparable to ratio of flac codec.

Examples:
========

~~~~~~~~{lisp}
(wavelet-audio:encode-wavelet-audio "~/file.wav" "~/file.wa")
(wavelet-audio:decode-wavelet-audio "~/file.wa" "~/file.wav")
~~~~~~~~

Documentation:
=============
Visit [the project page](http://shamazmazum.github.io/wavelet-audio)
on github pages.
