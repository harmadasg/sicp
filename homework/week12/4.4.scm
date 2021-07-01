(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (define (iter tests)
    (cond ((null? tests) #t)
	  ((null? (cdr tests)) (eval (car tests) env))
	  ((true? (eval (car tests) env)) (iter (cdr tests)))
	  (else #f)))
  (iter (cdr exp)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (define (iter tests)
    (if (null? tests)
	#f
	(let ((result (eval (car tests) env)))
	  (if (true? result)
	      result
	      (iter (cdr tests))))))
  (iter (cdr exp)))
  
(define (and->if exps)
  (cond ((null? exps) #t)
	((null? (cdr exps)) (car exps))
	(else (make-if (car exps)
		       (and->if (cdr exps))
		       #f))))

(define (or->if exps)
  (if (null? exps)
      #f
      (make-if (car exps)
	       (car exps)
	       (or->if (cdr exps)))))  
