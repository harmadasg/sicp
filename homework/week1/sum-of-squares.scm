#lang simply-scheme
(define (sum-square-two-largest a b c)
    (define (square n)
    (* n n))
    (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
          ((and (< b a) (< b c)) (+ (square a) (square c)))
          (else (+ (square a) (square b)))))