#lang simply-scheme
(define (filtered-accumulate combiner filter null-value term a next b)
    (if (> a b) null-value
        (combiner
         (if (filter a) (term a) null-value)
         (filtered-accumulate combiner filter null-value term (next a) next b))))

(define (prime? n)
  (define (prime-iter? i)
    (cond ((= n 1) #f)
          ((= i n) #t)
          ((= 0 (remainder n i)) #f)
          (else (prime-iter? (+ i 1)))))
  (prime-iter? 2))

(define (inc n)
  (+ n 1))

(define (sum-square-prime a b)
  (define (square n)
    (* n n))
    (filtered-accumulate + prime? 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

 (define (product-relative-primes n)
    (define (product-relative-primes-helper a b)
      (filtered-accumulate * (lambda (x) (= 1 (gcd x b))) 1 (lambda (n) n) a inc b))
    (product-relative-primes-helper 1 n))