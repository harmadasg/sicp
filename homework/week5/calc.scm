#lang simply-scheme
;; Scheme calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (print (calc-eval (read)))
  (newline)
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((word? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + (cdr args))))))
	((eq? fn '*) (accumulate * args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate * (cdr args))))))
        ((eq? fn 'first) (first (car args)))
        ((eq? fn 'bf) (bf (car args)))
        ((eq? fn 'last) (last (car args)))
        ((eq? fn 'bl) (bl (car args)))
	(else (error "Calc: bad operator:" fn))))