(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp) (map car (cadr exp)))
(define (let-values exp) (map cadr (cadr exp)))
(define let-body cddr)

(define (let->combination exp)
  (cons (make-lambda (let-parameters exp) (let-body exp)) (let-values exp)))
  
(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
        ((let? exp) (mc-eval (let->combination exp) env))
	((begin? exp) 
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (mc-eval (cond->if exp) env))
	((application? exp)
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))  