(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
  
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (an-element-of (cdr word-list))))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
	
; examples:	
;	
;	(sentence (noun-phrase (article the) (noun student)) (verb studies))
;	(sentence (noun-phrase (article the) (noun student)) (verb lectures))
;	(sentence (noun-phrase (article the) (noun student)) (verb eats))
;	(sentence (noun-phrase (article the) (noun student)) (verb sleeps))
;	(sentence (noun-phrase (article the) (noun professor)) (verb studies))
;	(sentence (noun-phrase (article the) (noun professor)) (verb lectures))
;	(sentence (noun-phrase (article the) (noun professor)) (verb eats))
;	(sentence (noun-phrase (article the) (noun professor)) (verb sleeps))
;	(sentence (noun-phrase (article the) (noun cat)) (verb studies))
	