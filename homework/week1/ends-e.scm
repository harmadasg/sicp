#lang simply-scheme
(define (ends-e s)
    (define (ends-e-transformer w)
      (if (equal? 'e (last w))
        w
        '()))
    (if (empty? s)
        '()
        (se (ends-e-transformer (first s))
            (ends-e (bf s)))))