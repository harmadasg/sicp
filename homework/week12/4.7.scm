(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp) (map car (cadr exp)))
(define (let-values exp) (map cadr (cadr exp)))
(define let-body cddr)
(define (make-let params values body)
  (list 'let
        (map (lambda (param value) (list param value)) params values)
        body))

(define (let->combination exp)
  (cons (make-lambda (let-parameters exp) (let-body exp)) (let-values exp)))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (helper params-values)
    (if (null? params-values)
        (caddr exp) ;;body
        (make-let (list (caar  params-values)) (list (cadar  params-values)) (helper (cdr  params-values)))))
  (helper (cadr exp)))