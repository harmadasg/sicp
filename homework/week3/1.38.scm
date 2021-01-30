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

(define e (+ 2 
             (cont-frac
              (lambda (k) 1.00)
              (lambda (k)
                (if (= 2 (remainder k 3))
                    (* 2 (round (/ k 3)))
                    1))
              100)))
