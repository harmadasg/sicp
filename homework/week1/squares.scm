#lang simply-scheme
(define (squares nums)
    (define (square num)
      (* num num))
    (if (empty? nums)
        '()
        (se (square (first nums))
            (squares (bf nums)))))