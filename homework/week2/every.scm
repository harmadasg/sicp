#lang simply-scheme
(define (every f sent)
    (if (empty? sent) '()
        (se (f (first sent)) (every f (bf sent)))))