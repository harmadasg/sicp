#lang simply-scheme
(define (number-of-partitions n)
    (define (count n i)
      (cond
        ((= n 0) 1)
        ((or (< n 0) (= i 0)) 0)
        (else (+
               (count n (- i 1))
               (count (- n i) i)))))
    (count n n))

