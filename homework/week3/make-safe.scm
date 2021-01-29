#lang simply-scheme
(define (make-safe f pred?)
    (lambda (x) (if (pred? x)
                    (f x)
                    #f)))