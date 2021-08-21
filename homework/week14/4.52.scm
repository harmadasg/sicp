; ((if-fail? exp) (analyze-if-fail exp))

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define if-fail-primary cadr)
(define if-fail-alternative caddr)

(define (analyze-if-fail exp)
  (let ((primary (analyze (if-fail-primary exp)))
        (alternative (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (primary env
             succeed
             (lambda () (alternative env succeed fail))))))