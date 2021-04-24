;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define-class (basic-object)
  (instance-vars (properties (make-table)))
  (method (put property value) (insert! property value properties))
  (default-method (lookup message properties)) )
  
(define-class (place name)
  (parent (basic-object))
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
	(for-each (lambda (person) (ask person 'notice new-person)) people)
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)
  (method (may-enter? person) #t)
  (method (place?) #t)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )
	
(define-class (locked-place name)
	(parent (place name))
	(instance-vars (locked? #t))
	(method (unlock) (set! locked? #f) 'unlocked)
	(method (may-enter? person) (not locked?)) )
	
(define-class (hotspot password)
	(parent (place 'hotspot))
	(instance-vars (connected-laptops '()))
	(method (gone thing)
		(usual 'gone thing) (set! connected-laptops (delete thing connected-laptops)))
	(method (connect laptop pw)
		(if (and (memq laptop (ask self 'things)) (eq? pw password))
			(set! connected-laptops (cons laptop connected-laptops)))
			'connected)
	(method (surf laptop url)
		(if (memq laptop connected-laptops)
			(system (string-append "lynx " url))
			(error "Not connected to network"))) )
	
(define-class (counter)
	(instance-vars (num 0))
	(method (count) (set! num (+ 1 num)) num) )

(define-class (garage)
	(parent (place 'garage))
	(class-vars (counter (instantiate counter)))
	(instance-vars (table (make-table)))
	(method (park vehicle)
		(if (not (memq vehicle (ask self 'things)))
			(error "Car is not in garage" ))
			(let ((ticket (instantiate ticket (ask counter 'count)))
				  (owner (ask vehicle 'possessor)))
					(insert! (ask ticket 'number) vehicle table)
					(ask owner 'lose vehicle)
					(ask self 'appear ticket)					
					(ask owner 'take ticket)))
	(method (unpark ticket)
		(if (not (eq? 'ticket (ask ticket 'name)))
			(error "Not a ticket"))
			(let ((num (ask ticket 'number)))
				(let ((vehicle (lookup num table)))
					(if (eq? vehicle #f)
					(error "No car found by ticket number"))
					(let ((owner (ask ticket 'possessor)))
						(ask owner 'lose ticket)
						(ask owner 'take vehicle)
						(insert! num #f table))))) )

(define-class (person name place)
  (parent (basic-object))
  (instance-vars
   (possessions '())
   (saying ""))
  (initialize
   (ask place 'enter self)
   (ask self 'put 'strength 6))
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions))
	       
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		   (ask pers 'lose thing)
		   (have-fit pers))))
	    (ask place 'people))
	       
	   (ask thing 'change-possessor self)
	   'taken)))

  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (person?) #t)
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
	     (error "Can't go" direction))
		((not (ask new-place 'may-enter? self)) (error "Can't enter new place"))
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self)))))
  (method (take-all) 
	(let ((things (ask place 'things)))
	  (let ((not-owned (filter (lambda (thing) (eq? (ask thing 'possessor) 'no-one)) things)))
		(for-each (lambda (thing) (ask self 'take thing)) not-owned)))) )

(define-class (thing name)
	(parent (basic-object))
	(instance-vars (possessor 'no-one))
	(method (type) 'thing)
	(method (thing?) #t)
	(method (change-possessor new-possessor) (set! possessor new-possessor)) )
	   
(define-class (ticket number)
	(parent (thing 'ticket)) )
	
(define-class (laptop)
	(parent (thing 'laptop))
	(method (connect password)
		(let ((owner (ask self 'possessor)))
			(if (not (eq? owner 'no-one))			
				(let ((place (ask owner 'place)))
					(ask place 'connect self password)))))
	(method (surf url)
		(let ((owner (ask self 'possessor)))
			(if (not (eq? owner 'no-one))			
				(let ((place (ask owner 'place)))
					(ask place 'surf self url))))))
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member? (ask thing 'name) *foods*))

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (ask obj 'person?)))

(define (thing? obj)
  (and (procedure? obj)
       (ask obj 'thing?)))
	   
(define (place? obj)
  (and (procedure? obj)
       (ask obj 'place?)))
	
; 2F
(define (whereis person)
	(if (person? person)
		(ask (ask person 'place) 'name)
		(error "Object not a person")))
		
(define (owner thing)
        (if (eq? 'thing (ask thing 'type))
			(let ((possessor (ask thing 'possessor)))
				(if (person? possessor)
					(ask possessor 'name)
					possessor))
           (error "Object not a thing")))		 
