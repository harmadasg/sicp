(define (list-of-values exps env)       ;; left to right
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
	(cons left (list-of-values (rest-operands exps) env)))))

(define (list-of-values exps env)       ;; right
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env) right))))