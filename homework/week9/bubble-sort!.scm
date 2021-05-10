#lang simply-scheme

(define (bubble-sort! vec)
  (define (loop i j)
    (if (= j 0)
        'done
        (if (> i j)
            (loop 1 (- j 1))
            (let ((prev (vector-ref vec (- i 1)))
                  (curr (vector-ref vec i)))
              (if (< prev curr)
                  (loop (+ i 1) j)
                  (begin
                    (vector-set! vec (- i 1) curr)
                    (vector-set! vec i prev)
                    (loop (+ i 1) j)))))))        
    (loop 1 (- (vector-length vec) 1)))

(define v1 (vector 9 8 7 6 5 4 3 2 1))
(bubble-sort! v1)