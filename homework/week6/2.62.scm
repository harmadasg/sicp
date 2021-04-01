#lang simply-scheme

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (if (and (null? set1) (null? set2))
      '()    
        (cond
          ((null? set2) (cons (car set1) (union-set (cdr set1) set2)))
          ((null? set1) (cons (car set2) (union-set set1 (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1) (union-set (cdr set1) set2)))
          ((> (car set1) (car set2))
           (cons (car set2) (union-set set1 (cdr set2))))
          ((= (car set1) (car set2))
           (cons (car set1) (union-set (cdr set1)
                                       (cdr set2)))))))

