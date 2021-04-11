#lang simply-scheme

(define f
  (let ((init 0))
    (lambda (n)
      (let ((old-value init))
        (set! init (+ init n)) old-value))))

(+ (f 0) (f 1))
