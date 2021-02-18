#lang simply-scheme

(define (compose f g)
    (lambda (x) (f (g x))))

(define (cxr-function wd)
    (define (iter f wd)
      (cond ((equal? (first wd) 'r) f)
            ((equal? (first wd) 'd) (iter (compose cdr f) (bf wd)))
            ((equal? (first wd) 'a) (iter (compose car f) (bf wd)))
            (else (error "Invalid input"))))
  (if (equal? (first wd) 'c)
      (iter (lambda (n) n) (bf wd))
      (error "Invalid input")))
