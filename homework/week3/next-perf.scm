#lang simply-scheme
(define (sum-of-factors n)
    (define (iter i result)
      (cond
        ((= 0 i) result)
        ((= (remainder n i) 0)
         (iter (- i 1) (+ result i)))
        (else (iter (- i 1) result))))
    (iter (round(/ n 2)) 0))

(define (next-perf n)
  (if (= n (sum-of-factors n))
      n
      (next-perf (+ n 1))))

