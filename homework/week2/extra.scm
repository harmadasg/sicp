#lang simply-scheme
(((lambda (f) (lambda (n) (f f n)))
   (lambda (fact n)
     (if (= n 1) 
         1
         (* n (fact fact (- n 1))))))
   5)