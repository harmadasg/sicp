#lang simply-scheme
(define (or-and-special-form)
  (define (infinite-loop x)
    (infinite-loop 10))
  (if
   (and (or (= 1 1) (infinite-loop 10))
        (not (and (= 1 2) (infinite-loop 10))))
  '(special form)
  '(not special form)))