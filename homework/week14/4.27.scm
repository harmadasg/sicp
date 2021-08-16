(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)
  
(define w (id (id 10)))
;;; L-Eval input:
count
;;; L-Eval value:
1
;;; L-Eval input:
w
;;; L-Eval value:
10
;;; L-Eval input:
count
;;; L-Eval value:
2

; When defining w only the outer id function gets evaluated (and count is incremented), the inner one is transformed to a thunk. When getting the value of w, the ; thunk gets evaluated and count is incremented once more.