(in-package :wavelet-audio-tests)

(def-suite io   :description "Input/output tests")
(def-suite full :description "Check encoding/decoding procedure")
(defparameter *compression-rates*
  (make-hash-table :test #'equalp))

(defun run-tests ()
  (prog1
      (every #'identity
             (mapcar (lambda (suite)
                       (let ((status (run suite)))
                         (explain! status)
                         (results-status status)))
                     '(io full)))
    (format t "Compression rates:~%")
    (maphash (lambda (key val)
               (destructuring-bind (channels . bps) key
                 (format t "~d channels, ~d bps: ~f%~%" channels bps val)))
             *compression-rates*)))

(in-suite io)
(test rice-code
  "Test Rice coder"
  (let ((sequence (loop for x from -30 to 30 collect x)))
    (loop for p from 1 below 32 do
         (let ((output
                (with-output-to-sequence (octet-stream)
                  (with-bit-output-stream (stream :callback (make-stream-output-callback octet-stream))
                    (loop for x in sequence do
                         (wavelet-audio::write-rice stream x 10))))))
           (is (equalp sequence
                       (with-input-from-sequence (octet-stream output)
                         (with-bit-input-stream (stream :callback (make-stream-input-callback octet-stream))
                           (loop repeat (length sequence) collect
                                (wavelet-audio::read-rice stream 10))))))))))

(test golomb-code
  "Test Golomb coder"
  (let ((sequence (loop for x from -30 to 30 collect x)))
    (loop for p from 1 below 15 do
         (let ((output
                (with-output-to-sequence (octet-stream)
                  (with-bit-output-stream (stream :callback (make-stream-output-callback octet-stream))
                    (loop for x in sequence do
                         (wavelet-audio::write-golomb stream x p))))))
           (is (equalp sequence
                       (with-input-from-sequence (octet-stream output)
                         (with-bit-input-stream (stream :callback (make-stream-input-callback octet-stream))
                           (loop repeat (length sequence) collect
                                (wavelet-audio::read-golomb stream p))))))))))

(test history
  "Test history tracking"
  (let ((history (wavelet-audio::make-history 5)))
    (is (= (wavelet-audio::history-sum history) 0))
    (wavelet-audio::history-insert history 1)
    (is (= (wavelet-audio::history-sum history) 1))
    (wavelet-audio::history-insert history 2)
    (is (= (wavelet-audio::history-sum history) 3))
    (wavelet-audio::history-insert history 3)
    (is (= (wavelet-audio::history-sum history) 6))
    (wavelet-audio::history-insert history 4)
    (is (= (wavelet-audio::history-sum history) 10))
    (wavelet-audio::history-insert history 5)
    (is (= (wavelet-audio::history-sum history) 15))
    (wavelet-audio::history-insert history 6)
    (is (= (wavelet-audio::history-sum history) 20))
    (wavelet-audio::history-insert history 7)
    (is (= (wavelet-audio::history-sum history) 25))))

(test adaptive-rice-code
  "Test adaptive Rice coder"
  (let ((sequence (loop for x from -30 to 30 collect x)))
    (let ((output
           (with-output-to-sequence (octet-stream)
             (with-bit-output-stream (stream :callback (make-stream-output-callback octet-stream))
               (loop
                  for x in sequence
                  with history = (wavelet-audio::make-history 10)
                  do
                    (wavelet-audio::adaptive-write history stream x))))))
      (is (equalp sequence
                  (with-input-from-sequence (octet-stream output)
                    (with-bit-input-stream (stream :callback (make-stream-input-callback octet-stream))
                      (loop
                         with history = (wavelet-audio::make-history 10)
                         repeat (length sequence) collect
                           (wavelet-audio::adaptive-read history stream)))))))))

(test peek-octet
  (let ((wavelet-audio::*buffer-len* 2))
    (with-input-from-sequence (stream #(0 1 2 3 4 5 6 7 8 9 10))
      (is (= (wavelet-audio::peek-octet stream 9) 9))
      (is (= (read-byte stream) 9)))))

(in-suite full)
(defun gen-sine (n &key (periods 1) (phase 0.0) (scale 255))
    (loop
       with array = (make-array n :element-type '(signed-byte 32))
       for i below n do
         (setf (aref array i)
               (truncate (* scale (sin (+ phase
                                          (/ (* 2 i periods pi)
                                             (1- n)))))))
       finally (return array)))

(defun test-codec (channels samplerate bps)
  "Test encoding/decoding of a block of mono audio, 8bps"
  (let* ((streaminfo (make-instance 'wavelet-audio:wavelet-audio-streaminfo
                                    :samplerate samplerate
                                    :bps bps
                                    :channels (length channels)
                                    :block-size (length (first channels))
                                    :samples (length (first channels))))
         (output
          (with-output-to-sequence (octet-stream)
            (with-bit-output-stream
                (stream :callback
                        (make-stream-output-callback octet-stream))
              (wavelet-audio:write-wavelet-audio-header stream (list streaminfo))
              (wavelet-audio:write-block
               stream
               (wavelet-audio:encode-block
                streaminfo
                (mapcar #'copy-seq channels)))
              (flush-bit-output-stream stream)))))

    (with-input-from-sequence (octet-stream output)
      (with-bit-input-stream (stream :callback (make-stream-input-callback octet-stream))
        (let* ((streaminfo-read (first (wavelet-audio:open-wavelet-audio stream)))
               (wa-block (wavelet-audio:read-block stream streaminfo-read))
               (channels-read (wavelet-audio:decode-block wa-block)))
          (is (every #'equalp channels channels-read))
          (is (= (wavelet-audio:streaminfo-bps streaminfo)
                 (wavelet-audio:streaminfo-bps streaminfo-read)))
          (is (= (wavelet-audio:streaminfo-channels streaminfo)
                 (wavelet-audio:streaminfo-channels streaminfo-read)))
          (is (= (wavelet-audio:streaminfo-samplerate streaminfo)
                 (wavelet-audio:streaminfo-samplerate streaminfo-read)))
          (is (= (wavelet-audio:streaminfo-samples streaminfo)
                 (wavelet-audio:streaminfo-samples streaminfo-read))))))
    (setf (gethash (cons (length channels) bps) *compression-rates*)
          (* 100.0
             (let ((old-size (reduce #'+ (mapcar #'length channels)))
                   (new-size (length output)))
               (/ (- old-size new-size) old-size))))))

(test codec-s16-mono
  "Test mono s16 audio"
  (test-codec
   (list
    (gen-sine 4096
              :periods 3
              :scale (1- (ash 1 15))))
   44100 16))

(test codec-s8-mono
  "Test mono s8 audio"
  (test-codec
   (list
    (gen-sine 4096
              :periods 3
              :scale (1- (ash 1 7))))
   44100 8))

(test codec-s16-stereo
  "Test stereo s16 audio"
  (test-codec
   (list
    (gen-sine 4096
              :periods 3
              :scale (1- (ash 1 15)))
    (gen-sine 4096
              :periods 3
              :scale (1- (ash 1 15))
              :phase 0.1))
   44100 16))

(test codec-s8-stereo
  "Test stereo s8 audio"
  (test-codec
   (list
    (gen-sine 4096
              :periods 3
              :scale (1- (ash 1 7)))
    (gen-sine 4096
              :periods 3
              :scale (1- (ash 1 7))
              :phase 0.1))
   44100 8))
