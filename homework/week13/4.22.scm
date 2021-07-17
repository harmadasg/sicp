(define (analyze-let exp)
  (let ((new-exp (let->combination exp)))
    (analyze new-exp)))