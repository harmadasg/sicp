#lang simply-scheme

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (best-total cards)
    (define (iter total aces cards)
      (if (not (empty? cards))
             (let ((value (bl (first cards))))
               (cond               
                 ((number? value) (iter (+ total value) aces (bf cards)))
                 ((or (equal? value 'a) (equal? value 'A)) (iter (+ total 11) (+ aces 1) (bf cards)))
                 (else (iter (+ total 10) aces (bf cards)))))
             (if (and (> total 22) (> aces 0))
                 (iter (- total 10) (- aces 1) cards)
                 total)))
    (iter 0 0 cards))

(define (stop-at-17 customer-hand-so-far dealer-up-card)
    (< (best-total customer-hand-so-far) 17))

(define (play-n strategy n)
    (if (= n 0)
        0
        (+ (twenty-one strategy) (play-n strategy (- n 1)))))

(define (dealer-sensitive customer-hand-so-far dealer-up-card)
    (let ((dealer-more-than-6 (> (best-total (se dealer-up-card)) 6))
          (customer-less-than-17 (stop-at-17 customer-hand-so-far dealer-up-card))
          (dealer-less-than-7 (< (best-total (se dealer-up-card)) 7))
          (customer-less-than-12 (< (best-total customer-hand-so-far) 12)))
      (let ((first-case (and dealer-more-than-6 customer-less-than-17))
            (second-case (and dealer-less-than-7 customer-less-than-12)))
        (or first-case second-case))))

(define (stop-at n)
    (lambda (customer-hand-so-far dealer-up-card)
      (< (best-total customer-hand-so-far) n)))