(define adjectives '(adjective beautiful cool old happy))
(define adverbs '(adverb loudly impatiently badly enormously))		
		
(define (parse-simple-noun-phrase)
  (amb
    (list 'simple-noun-phrase
        (parse-word articles)
		(parse-word adjectives)
        (parse-word nouns))
    (list 'simple-noun-phrase
          (parse-word articles)
          (parse-word nouns))))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))  


(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
		 (maybe-extend (list 'adverb-phrase
							 verb-phrase
							 (parse-word adverbs)))
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))