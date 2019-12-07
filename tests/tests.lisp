(in-package :wavelet-audio-tests)

(def-suite wavelet :description "Wavelet transform tests")
(def-suite io :description "Input/output tests")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(wavelet io))))

(in-suite wavelet)
(test transform
  "Test wavelet transform"
  (let* ((array (make-array 2048
                            :element-type '(signed-byte 32)
                            :initial-contents (loop repeat 2048 collect (- (random 500) 1000))))
         (array2 (copy-seq array)))
    (setq array2 (wavelet-transform:wavelet-inverse (wavelet-transform:wavelet-forward array2)))
    (is (equalp array array2))))

(test transform-recopy
  "Test wavelet transform with recopy"
  (let* ((array (make-array 2048
                            :element-type '(signed-byte 32)
                            :initial-contents (loop repeat 2048 collect (- (random 500) 1000))))
         (array2 (copy-seq array)))
    (setq array2 (wavelet-transform:wavelet-inverse-w/recopy
                  (wavelet-transform:wavelet-forward-w/recopy array2)))
    (is (equalp array array2))))

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
