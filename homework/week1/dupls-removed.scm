#lang simply-scheme
(define (dupls-removed s)
  (define (dupls-removed-iter s temp)
    (cond ((empty? s) temp)
          ((member? (first s) temp) (dupls-removed-iter (bf s) temp))
          (else (dupls-removed-iter (bf s) (se temp (first s))))))
    (dupls-removed-iter s '()))