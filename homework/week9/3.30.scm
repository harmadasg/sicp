#lang sicp

(define (ripple-carry-adder a-list b-list s-list c-in)
  (define (helper a-list b-list s-list c-in)
    (if (null? a-list)
        'ok
    (let ((c-out (make-wire)))
      (full-adder (car a-list) (car b-list) c-in (car s-list) c-out)
      (helper (cdr a-list) (cdr b-list) (cdr s-list) c-out))))
  (helper a-list b-list s-list c-in))

