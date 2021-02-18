#lang simply-scheme
(define (for-each f l)
    (if (null? l)
        #t
        (let ((curr (f (car l))))
          (for-each f (cdr l)))))
