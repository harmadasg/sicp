;;; logo.scm         part of programming project #4


;;; Problem A1   make-line-obj

(define-class (line-obj line)
  (method (empty?) (null? line))
  (method (next) (let ((token (car line))) (set! line (cdr line)) token))
  (method (put-back token) (set! line (cons token line)) 'ok) )
  
 (define (make-line-obj text)
	(instantiate line-obj text))

;;; Problem A2   logo-type

(define (logo-type val)
  (cond ((null? val) (display ""))
        ((word? val) (display val))
        (else (display-list val)))
   '=no-value=)

(define (display-list val)
  (let ((elem (car val)))
    (cond ((and (word? elem) (last? val)) (logo-type elem))
          ((and (list? elem) (last? val)) (display "[") (logo-type elem) (display "]"))
          ((word? elem) (logo-type elem) (display " ") (display-list (cdr val)))
          ((list? elem) (display "[") (logo-type elem) (display "] ") (display-list (cdr val)))
          (else (error "Invalid argument:" elem)))))

 (define (last? lst)
   (null? (cdr lst)))

(define (logo-print val)   
  (logo-type val)  
  (newline) 
  '=no-value=) 

(define (logo-show val)   
  (logo-print (list val)))   



;;; Problem 4   variables   (logo-meta.scm is also affected)

(define (make env var val) 
  (set-variable-value! var val env)
  '=no-value=) 


;;; Here are the primitives RUN, IF, and IFELSE.  Problem B2 provides
;;; support for these, but you don't have to modify them.   

(define (run env exp)
  (eval-line (make-line-obj exp) env))

(define (logo-if env t/f exp) 
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp) env))
        ((eq? t/f 'false) '=no-value=)
        (else (error "Input to IF not true or false " t/f))))  

(define (ifelse env t/f exp1 exp2)  
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp1) env))
        ((eq? t/f 'false) (eval-line (make-line-obj exp2) env))   
        (else (error "Input to IFELSE not true or false " t/f)))) 

(define (test env t/f) 
  (cond ((eq? t/f 'true) (set-variable-value! '" test" 'true env))
        ((eq? t/f 'false) (set-variable-value! '" test" 'false env))   
        (else (error "Input to TEST not true or false " t/f))) 
  '=no-value=)

(define (iftrue env exp)
  (let ((test (find-test-value env))) 
    (cond ((eq? test 'true) (eval-line (make-line-obj exp) env))
		  ((eq? test 'false) '=no-value=)
		  (else (error "iftrue without TEST")))))

(define (iffalse env exp)
  (let ((test (find-test-value env))) 
    (cond ((eq? test 'true) '=no-value=)
		  ((eq? test 'false) (eval-line (make-line-obj exp) env))
		  (else (error "iffalse without TEST")))))

(define (find-test-value env)
	(let ((value (lookup-variable-value '" test" env)))
		  (cond ((or (eq? value 'true) (eq? value 'false)) value)
			    ((eq? env the-global-environment) value)
			    (else (find-test-value (enclosing-environment env))))))

;;; Problem B2   logo-pred

(define (logo-pred pred)
  (lambda args
    (if (apply pred args)
        'true
        'false)))

;;; Here is an example of a Scheme predicate that will be turned into  
;;; a Logo predicate by logo-pred:  

(define (equalp a b)
  (if (and (number? a) (number? b))  
      (= a b)   
      (equal? a b)))   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;  Stuff below here is needed for the interpreter to work but you  ;;;  
;;;  don't have to modify anything or understand how they work.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;;; The Logo reader

(define left-paren-symbol (string->symbol (make-string 1 #\( )))
(define right-paren-symbol (string->symbol (make-string 1 #\) )))
(define quote-symbol (string->symbol (make-string 1 #\" )))

(define (logo-read)  
  (define lookahead #f)   
  (define (logo-read-help depth)   
    (define (get-char)  
      (if lookahead  
          (let ((char lookahead))   
            (set! lookahead #f)   
            char) 
          (let ((char (read-char)))   
            (if (eq? char #\\)
                (list (read-char))  
                char)))) 
    (define (quoted char)   
      (if (pair? char)   
          char 
          (list char)))  
    (define (get-symbol char)   
      (define (iter sofar char)
        (cond ((pair? char) (iter (cons (car char) sofar) (get-char))) 
              ((memq char  
                     '(#\space #\newline #\+ #\- #\* #\/  
                               #\= #\< #\> #\( #\) #\[ #\] ))
               (set! lookahead char)   
               sofar) 
              (else (iter (cons char sofar) (get-char))) ))   
      (string->word (list->string (reverse (iter '() char)))) )
    (define (get-token space-flag)   
      (let ((char (get-char)))   
              (cond ((eq? char #\space) (get-token #t))  
              ((memq char '(#\+ #\* #\/ #\= #\< #\> #\( #\) ))   
               (string->symbol (make-string 1 char)))
              ((eq? char #\-)   
               (if space-flag  
                   (let ((char (get-char)))   
                     (let ((result (if (eq? char #\space)  
                                       '- 
                                       '=unary-minus=))) 
                       (set! lookahead char)   
                       result)) 
                   '-)) 
              ((eq? char #\[) (logo-read-help (+ depth 1)))  
              ((pair? char) (get-symbol char))
              ((eq? char #\")   
               (let ((char (get-char)))   
                 (if (memq char '(#\[ #\] #\newline))  
                     (begin (set! lookahead char) quote-symbol)
                     (string->symbol (word quote-symbol
					   (get-symbol (quoted char)))))))
	      (else (get-symbol char)) )))

    (define (after-space)
      (let ((char (get-char)))
	(if (eq? char #\space)
	    (after-space)
	    char)))
    (let ((char (get-char)))   
      (cond ((eq? char #\newline)
             (if (> depth 0) (set! lookahead char))   
             '()) 
	    ((eq? char #\space)
	     (let ((char (after-space)))
	       (cond ((eq? char #\newline)
		      (begin (if (> depth 0) (set! lookahead char))
			     '()))
		     ((eq? char #\])
		      (if (> depth 0) '() (error "Unexpected ]")))
		     (else (set! lookahead char)
			   (let ((token (get-token #t)))
			     (cons token (logo-read-help depth)))))))
            ((eq? char #\])   
             (if (> depth 0) '() (error "Unexpected ]")))
            ((eof-object? char) char)   
            (else (set! lookahead char)
                  (let ((token (get-token #f)))
                    (cons token (logo-read-help depth)) ))))) 
  (logo-read-help 0))  


;;; Assorted stuff   

(define (make-logo-arith op)   
  (lambda args (apply op (map maybe-num args))))   

(define (maybe-num val)
  (if (word? val)
      (string->word (word->string val))
      val))

(define tty-port (current-input-port))   

(define (prompt string)   
  (if (eq? (current-input-port) tty-port)
  (begin (display string) (flush))))  

(define (meta-load fn)   
  (define (loader)  
    (let ((exp (logo-read)))   
      (if (eof-object? exp)   
          '() 
          (begin (eval-line (make-line-obj exp)
			    the-global-environment) 
		 (loader))))) 
  (with-input-from-file (symbol->string fn) loader)
  '=no-value=) 
