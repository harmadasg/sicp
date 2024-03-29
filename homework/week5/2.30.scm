#lang simply-scheme
(define (square n)
  (* n n))
       
(define (square-tree tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree2 tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (square-tree2 sub-tree)
               (square sub-tree)))
         tree))