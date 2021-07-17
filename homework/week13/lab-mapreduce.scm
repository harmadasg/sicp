; 1)
(define total-lines
	(stream-accumulate + 0 (stream-map kv-value 
		(mapreduce (lambda (input-key-value-pair)
			(list (make-kv-pair (kv-key input-key-value-pair) 1))
			+
			0
			"/gutenberg/shakespeare")))))
			
; 2a)
(define gutenberg-wordcounts
	(mapreduce (lambda (input-key-value-pair)
		(map (lambda (word) (make-kv-pair word 1)) (kv-value input-key-value-pair))) 
		+
		0
		"/gutenberg/shakespeare"))
		
; 2b)
(define (find-max-mapper kv-pair)
	(list (make-kv-pair (first (kv-key kv-pair))
	kv-pair)))
	
(define (find-max-reducer current so-far)
	(if (> (kv-value current) (kv-value so-far))
	current
	so-far))
	
(define frequent (mapreduce find-max-mapper find-max-reducer
	(make-kv-pair ’foo 0) gutenberg-wordcounts))

(stream-accumulate find-max-reducer (make-kv-pair ’foo 0)
	(stream-map kv-value frequent))

; 2c)
(define words-used-once
	(stream-filter (lambda (kv-pair) (= 1 (kv-value kv-pair))) gutenberg-wordcounts))

; 3a)
(define (grep pattern directory)
	(mapreduce (lambda (kv-pair) (if (match? pattern (kv-value kv-pair)) (list kv-pair) '()))
		 cons-stream the-empty-stream directory))	
