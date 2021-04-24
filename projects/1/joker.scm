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
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) 'J 'J))

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 54) )

(define (best-total cards)
    (define (iter total aces jokers cards)
      (if (not (empty? cards))
             (let ((value (bl (first cards))))
               (cond
                 ((empty? value) (iter (+ total 11) aces (+ 1 jokers) (bf cards)))
                 ((number? value) (iter (+ total value) aces jokers (bf cards)))
                 ((equal? value 'A) (iter (+ total 11) (+ aces 1) jokers (bf cards)))
                 (else (iter (+ total 10) aces jokers (bf cards)))))
             (cond
               ((and (> total 22) (> jokers 0))(iter 21 aces (- jokers 1) cards))
               ((and (> total 22) (> aces 0))(iter (- total 10) (- aces 1) jokers cards))
                 (else total))))
    (iter 0 0 0 cards))

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

(define (valentine customer-hand-so-far dealer-up-card)
    (define (contains-heart cards)
      (> (length (keep (lambda (card) (member? 'H card)) cards)) 0))
    (if (contains-heart customer-hand-so-far)
        (< (best-total customer-hand-so-far) 19)
        (< (best-total customer-hand-so-far) 17)))

(define (suit-strategy suit default-strategy strategy-with-suit)
    (define (contains-suit cards)
      (> (length (keep (lambda (card) (member? suit card)) cards)) 0))
    (lambda (customer-hand-so-far dealer-up-card)
      (if (contains-suit customer-hand-so-far)
          (strategy-with-suit customer-hand-so-far dealer-up-card)
          (default-strategy customer-hand-so-far dealer-up-card))))

(define (valentine2 customer-hand-so-far dealer-up-card)
    ((suit-strategy 'H (stop-at 17) (stop-at 19)) customer-hand-so-far dealer-up-card))

(define (majority strategy1 strategy2 strategy3)
    (lambda (customer-hand-so-far dealer-up-card)
      (if (strategy1 customer-hand-so-far dealer-up-card)
          (or (strategy2 customer-hand-so-far dealer-up-card) (strategy3 customer-hand-so-far dealer-up-card))
          (and (strategy2 customer-hand-so-far dealer-up-card) (strategy3 customer-hand-so-far dealer-up-card)))))

(define (reckless strategy)
    (lambda (customer-hand-so-far dealer-up-card)
      (if (= (best-total customer-hand-so-far) 21)
          #f
          (strategy (bl customer-hand-so-far ) dealer-up-card))))