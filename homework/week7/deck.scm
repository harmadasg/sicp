(define ordered-deck ’(AH 2H 3H ... QH KH AS 2S ... QC KC))

(define (shuffle deck)
(if (null? deck)
’()
(let ((card (nth (random (length deck)) deck)))
(cons card (shuffle (remove card deck))) )))

(define-class (deck)
  (instance-vars (shuffled-deck (shuffle ordered-deck)))
  (method (deal) (if (ask self 'empty?) null (let ((top (car shuffled-deck))) (set! shuffled-deck (cdr shuffled-deck)) top)))
  (method (empty?)
	(null? shuffled-deck)) )