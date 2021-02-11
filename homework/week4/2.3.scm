#lang simply-scheme

(define (make-point x y)
    (cons x y))
(define (x-point point)
    (car point))
(define (y-point point)
    (cdr point))
(define (make-segment a b)
    (cons a b))
(define (start-segment segment)
    (car segment))
(define (end-segment segment)
    (cdr segment))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
    (make-point 
     (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
     (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (rect-perimeter rect)
    (* (+ (a-side rect) (b-side rect)) 2))
(define (rect-area rect)
    (* (a-side rect) (b-side rect)))

(define (make-rect top-left bottom-right)
    (cons top-left bottom-right))
(define (a-side rect)
    (+ (abs (- (x-point (car rect)) (x-point (cdr rect)))) 1))
(define (b-side rect)
    (+ (abs (- (y-point (car rect)) (y-point (cdr rect)))) 1))