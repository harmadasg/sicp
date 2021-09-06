;(define (last-pair lst)
;	(if (null? (cdr lst))
;		lst
;		(last-pair (cdr lst)))

(rule (last-pair (?a . ()) (?a . ())))

(rule (last-pair (?u . ?v) ?z)
	(last-pair ?v ?z))