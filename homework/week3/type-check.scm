#lang simply-scheme
(define (type-check f pred? arg)
    (if (pred? arg)
        (f arg)
        #f))