#lang simply-scheme
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cadr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))

(define (branch-weight branch)
      (if (number? (branch-structure branch))
          (branch-structure branch)
          (total-weight (branch-structure branch))))

(define (total-weight mobile)  
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (branch-product branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define x (make-mobile (make-branch 1 4) (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 2)))))

(define y (make-mobile (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 2))) (make-branch 1 5)))

(define (balanced? mobile)
    (define (helper mobile)
      (= (branch-product (left-branch mobile))
         (branch-product (right-branch mobile))))
  (and (helper mobile)
       (if (contains-mobile? (left-branch mobile))  (helper (branch-structure(left-branch mobile))) #t)
       (if (contains-mobile? (right-branch mobile))  (helper (branch-structure (right-branch mobile))) #t)))

(define (contains-mobile? branch)
  (not (number? (branch-structure branch))))

         

