#lang simply-scheme
(define (ordered? nums)
    (cond ((= (length nums) 1) #t)
          ((> (first nums) (first (bf nums))) #f)         
          (else (ordered? (bf nums)))))