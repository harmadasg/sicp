#lang sicp

(define (partial-sums s)
  (define result (cons-stream (stream-car s)
                              (add-streams (stream-cdr s) result)))
  result)
