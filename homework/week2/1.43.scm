#lang simply-scheme
(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1))x)))))

(define (square n)
  (* n n))
