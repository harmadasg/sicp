(define (make-frame variables values)
  (attach-tag 'frame (map cons variables values)))

(define contents cdr)

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (contents frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((record (assoc var (contents (first-frame env)))))
        (if record
            (cdr record)
            (lookup-variable-value var (enclosing-environment env))))))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
     (error "Unbound variable -- SET!" var)
      (let ((record (assoc var (contents (first-frame env)))))
        (if record
            (set-cdr! record val)
            (lookup-variable-value var (enclosing-environment env))))))

(define (define-variable! var val env)
  (let ((record (assoc var (contents (first-frame env)))))
    (if record
        (set-cdr! record val)
         (add-binding-to-frame! var val (first-frame env)))))
