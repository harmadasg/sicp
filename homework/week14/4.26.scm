(define (unless->if exp)
  (make-if (unless-predicate exp)
	   (unless-consequent exp)
	   (unless-alternative exp)))

(define unless-predicate cadr)
(define unless-alternative caddr)
(define unless-consequent cadddr)

; inside eval 
;((unless? exp) (eval (unless->if exp) env))