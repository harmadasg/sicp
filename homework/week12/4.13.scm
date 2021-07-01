(define (unbound? exp)
   (tagged-list? exp 'make-unbound!))

(define (eval-unbound exp env)
  (make-unbound! (cadr exp) env)
  'ok)

(define (make-unbound! var env)
  (if (eq? env the-empty-environment)
     (error "Unbound variable -- UNBOUND!" var)
      (let ((record (assoc var (contents (first-frame env)))))
        (if record
            (set-car! record nil)
            (make-unbound! var (enclosing-environment env))))))