#lang simply-scheme

(define (vector-append vec1 vec2)
    (define (loop newvec n1 n2)
      (cond
        ((and (< n1 0) (< n2 0)) newvec)
        ((>= n1 0)  (vector-set! newvec n1 (vector-ref vec1 n1)) (loop newvec (- n1 1) n2))
        ((>= n2 0)  (vector-set! newvec (+ n2 (vector-length vec1)) (vector-ref vec2 n2)) (loop newvec n1 (- n2 1)))))
      (loop (make-vector (+ (vector-length vec1) (vector-length vec2))) (- (vector-length vec1) 1) (- (vector-length vec2) 1)))

(define (vector-filter pred vec)
  (define (loop newvec n)
    (cond ((< n 0) newvec)
          ((pred (vector-ref vec n)) (loop (vector-append (vector (vector-ref vec n)) newvec) (- n 1)))
          (else (loop newvec (- n 1)))))     
  (loop (vector) (- (vector-length vec) 1)))
  