(define (if? exp)
  (and (tagged-list? exp 'if)
       (eq? (caddr exp) 'then)
       (or (= (length exp) 4)
	   (eq? (list-ref exp 4) 'else))))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (cadddr exp))

(define (if-alternative exp) (list-ref exp 5))