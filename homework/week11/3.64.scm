#lang sicp

(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (let ((diff (abs (- s0 s1))))
      (if (< diff tolerance)
          s1
          (stream-limit (stream-cdr s) tolerance)))))
      
  