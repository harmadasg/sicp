; 1a)
(define (inverted-mapper document-line-kv-pair)
  (map (lambda (wd-in-line) (make-kv-pair wd-in-line (kv-key document-line-kv-pair)))
       (kv-value document-line-kv-pair)))
	   
(define (distinct-reducer current so-far)
  (if (memq current so-far)
      so-far
      (cons-stream current so-far)))	   

(define (inverted-index directory)
	(mapreduce inverted-mapper distinct-reducer the-empty-stream directory))
	
; 1b)
(define (filtered-inverted-index min-length directory)
	(mapreduce
		(lambda (kv-pair)
			(inverted-mapper (make-kv-pair (kv-key kv-pair)
				(stream-filter (lambda (wrd) (>= (count wrd) min-length)) (kv-value kv-pair)))))
		distinct-reducer the-empty-stream directory))
		
; 2a)
(define from-address car)
(define to-address cadr)
(define subject caddr)		
		
(define (subject-occurrences-mapper email-data)
  (list (make-kv-pair (subject email-data) 1)))

(define subject-occurrences
	(mapreduce subject-occurrences-mapper + 0 "/sample-emails"))
	
; 2b)
(define (top n stream)
  (if (= n 0)
      '()
      (cons (stream-car stream)
            (top (- n 1) (stream-cdr stream)))))

(define most-common-subjects	
	(map cdr (top 10
		(mapreduce (lambda (kv-pair) (list (make-kv-pair (* (kv-value kv-pair) -1) (kv-key kv-pair))))
			cons-stream the-empty-stream subject-occurrences))))

; 2c)
(define (spam-mapper  email-data)
	(if (memq (subject email-data) most-common-subjects)
		(list (make-kv-pair (from-address email-data) 1))
		'()))
		
(define spammers
	(mapreduce spam-mapper + 0 "/sample-emails"))				