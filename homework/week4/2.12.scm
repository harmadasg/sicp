#lang simply-scheme
(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
	   (>= (upper-bound y) 0))
      (error "Can't divide by an interval that spans zero.")
      (mul-interval x
		    (make-interval (/ 1 (upper-bound y))
				   (/ 1 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
				 (- (lower-bound y)))))

(define (make-center-percent center percent)
    (let ((tolerance (* center (/ percent 100))))    
    (make-interval
     (- center tolerance)
     (+ center tolerance))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
    (- (* 100 (/ (upper-bound i) (center i))) 100))