(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	(else (let ((form (get (operator exp) 'eval)))
		 (if form
		     (form exp env)
		     (apply (eval (operator exp) env)
			    (list-of-values (operands exp) env) ))))))