#lang simply-scheme
 (define (symbols-equal? a b)
    (and (symbol? a) (symbol? b) (equal? a b)))

(define (lists-equal? a b)
  (if (null? a)
      #t
      (and (list? a) (list? b) (eq? (car a) (car b)) (eq? (cdr a)  (cdr b)))))

(define (eq? a b)
    (if (symbol? a)
        (symbols-equal? a b)
        (lists-equal? a b)))

