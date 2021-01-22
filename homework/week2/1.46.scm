#lang simply-scheme

(define (average x y)
  (/ (+ x y) 2))

(define (square n)
  (* n n))

(define (iterative-improve good-enough? improve)
  (define (inner-procedure guess)
    (if (good-enough? guess) guess
        (inner-procedure (improve guess))))
  inner-procedure)

(define (sqrt x)
  ((iterative-improve 
   (lambda (guess) (< (abs (- (square guess) x)) 0.001))
   (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
    ((iterative-improve
     (lambda (guess) (let ((next (f guess))) (< (abs (- next guess)) 0.00001)))
     (lambda (guess) (f guess)))
     1.0))