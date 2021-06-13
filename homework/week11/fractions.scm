#lang sicp

(define (fract-stream fraction)
  (let ((num (car fraction))
        (denom (cadr fraction)))
        (cons-stream (quotient (* 10 num) denom)
                     (fract-stream (list (remainder (* 10 num) denom) denom)))))

(define (approximation s numdigits)
  (if (= 0 numdigits)
      '()
      (cons
       (stream-car s)
       (approximation (stream-cdr s) (- numdigits 1)))))
         
     
      
  