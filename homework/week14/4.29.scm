; e.g.
(define (do-n-times n exp)
	(if (= n 0)
		'done
		(begin (exp) (do-n-times (- n 1) exp))))

;without memoization

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
2

;with memoization

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1