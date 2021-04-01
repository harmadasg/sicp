#lang simply-scheme

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (define (iter set1 set2)
    (if (null? set2)
        set1
        (iter (adjoin-set (car set2) set1) (cdr set2))))
    (iter set1 set2))