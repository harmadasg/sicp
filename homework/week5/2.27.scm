#lang simply-scheme
(define (deep-reverse lst)
  (define (iter old new)
    (cond ((null? old) new)
	  ((not (pair? old)) old)
	  (else (iter (cdr old) (cons (deep-reverse (car old)) new)))))
  (iter lst '()))