#lang simply-scheme
(define (square n)
  (* n n))
       
(define (map-tree f tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (f tree))
          (else (cons (map-tree f (car tree)) (map-tree f (cdr tree))))))

(define (map-tree2 f tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (map-tree2 f sub-tree)
               (f sub-tree)))
         tree))