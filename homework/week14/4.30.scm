; a) the example works because the supplied lambda contains two primitives (newline and display), so the arguments are strictly evaluated just as in applicative-order Scheme.

; b)
; Ben: (p1 1) = (1 2) (p2 1) = (1)   in this version the expression (set! x (cons x '(2))) becomes a thunk and never gets called
; Cy:  (p1 1) = (1 2) (p2 1) = (1 2) works as expected

; c)
; the lazy evaluator only delays procedure arguments, procedures are carried out normally. we only got a funny result in b) because the set! expression is inside a procedure argument.

; d) I think Cy is right, this way the lazy version behaves similarly as the applicative-order Scheme.