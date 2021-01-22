#lang simply-scheme
(define (product term a next b)
    (if (> a b) 1
        (* (term a) (product term (next a) next b))))

(define (factorial n)
  (define (inc n)
    (+ n 1))
    (product (lambda (x) x) 1 inc n))

(define (pi-term n)
    (* (/ (+ 1 n) (+ 2 n)) (/ (+ 3 n) (+ 2 n))))

(define (calc-pi n)
    (* 4 (product pi-term 1 (lambda (x) (+ x 2)) (* n 2))))
