#lang simply-scheme
(define (cont-frac n d k)
    (if (= k 0)
        0
        (/ (n 1) (+ (d 1) (cont-frac (lambda (x) (n (+ x 1)))
                                     (lambda (x) (d (+ x 1)))
                                     (- k 1))))))

(define (cont-frac-iter n d k)
    (define (iter ret k)
      (if (= k 0)
          ret
          (iter (/ (n k) (+  (d k) ret)) (- k 1))))
    (iter 1 k))
