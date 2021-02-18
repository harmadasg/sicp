#lang simply-scheme
(define (substitute lis old new)
    (cond ((null? lis) null)
          ((equal? (car lis) old) (cons new (substitute (cdr lis) old new)))
          ((list? (car lis)) (cons (substitute (car lis) old new) (substitute (cdr lis) old new)))
          (else (cons (car lis) (substitute (cdr lis) old new)))))

