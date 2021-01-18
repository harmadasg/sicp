#lang simply-scheme
(define (product term a next b)
    (if (> a b) 1
        (* (term a) (product term (next a) next b))))

(define (factorial n)
    (product (lambda (x) (* 1 x)) 1 (lambda (x) (+ x 1)) n))

(define (pi-term n)
    (* (/ (+ 1 n) (+ 2 n)) (/ (+ 3 n) (+ 2 n))))

(define (calc-pi n)
    (* 4 (product pi-term 1 (lambda (x) (+ x 2)) n)))