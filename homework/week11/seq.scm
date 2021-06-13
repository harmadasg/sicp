#lang sicp
(define (next-num n)
  (if (even? n)
      (/ n 2)
      (+ (* n 3) 1)))

(define (num-seq n)
  (cons-stream n (num-seq (next-num n))))

(define (seq-length s)
  (if (= (stream-car s) 1)
      1
      (+ 1 (seq-length (stream-cdr s)))))
