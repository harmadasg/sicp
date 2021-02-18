#lang simply-scheme
(define (substitute2 lis old new)
    (cond ((null? lis) null)
          ((list? (car lis)) (cons (substitute2 (car lis) old new) (substitute2 (cdr lis) old new)))
          ((member? (car lis) old) (cons
                                    (item (index-of (car lis) old) new)
                                    (substitute2 (cdr lis) old new)))         
          (else (cons (car lis) (substitute2 (cdr lis) old new)))))

(define (index-of w lst)
    (define (iter index lst)
      (cond ((not (member? w lst)) -1)
            ((equal? (car lst) w) index)
            (else (iter (+ index 1) (cdr lst)))))
    (iter 1 lst))