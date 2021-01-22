#lang simply-scheme
(define  (substitute sen old new)
    (if (empty? sen)
        '()
    (se
     (if (equal? (first sen) old) new (first sen))
     (substitute (bf sen) old new))))