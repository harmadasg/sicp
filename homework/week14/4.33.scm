(define (lazy-cons x y env)
  (make-procedure '(m) (list (list 'm x y)) env))
  
(define (process-quotation quoted env)
  (if (pair? quoted)
      (lazy-cons (process-quotation (car quoted) env)
		 (process-quotation (cdr quoted) env)
		 env)
      quoted))

; inside eval ((quoted? exp) (process-quotation (text-of-quotation exp) env))	  