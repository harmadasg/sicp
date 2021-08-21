; The problem with using an-integer-starting-from is that the first amb never fails in the original definition. So it has to be rewritten like this:

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
  
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))  
  
(define (a-pythagorean-triple-from n)
  (let ((k (an-integer-starting-from n)))
    (let ((j (an-integer-between n k)))
      (let ((i (an-integer-between n j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))