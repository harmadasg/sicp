#lang simply-scheme
(define (count-change amount)
  (cc amount 5 1))
(define (cc amount kinds-of-coins index)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (> index kinds-of-coins)) 0)
        (else (+ (cc amount kinds-of-coins
                     (+ index 1))
                 (cc (- amount
                        (first-denomination index))
                     kinds-of-coins index)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))