;;;  Data for adventure game.  This file is adv-world.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Soda (instantiate place 'Soda))
(define BH-Office (instantiate place 'BH-Office))
(define MJC-Office (instantiate place 'MJC-Office))
(define art-gallery (instantiate place 'art-gallery))
(define Pimentel (instantiate place 'Pimentel))
(define 61A-Lab (instantiate place '61A-Lab))
(define Sproul-Plaza (instantiate place 'Sproul-Plaza))
(define Telegraph-Ave (instantiate place 'Telegraph-Ave))
(define Noahs (instantiate restaurant 'Noahs bagel 0.50))
(define Intermezzo (instantiate place 'Intermezzo))
(define s-h (instantiate place 'sproul-hall))


(can-go Soda 'up art-gallery)
(can-go art-gallery 'down Soda)
(can-go art-gallery 'west BH-Office)
(can-go BH-Office 'east art-gallery)
(can-go art-gallery 'east MJC-Office)
(can-go MJC-office 'west art-gallery)
(can-go Soda 'south Pimentel)
(can-go Pimentel 'north Soda)
(can-go Pimentel 'south 61A-Lab)
(can-go 61A-Lab 'north Pimentel)
(can-go 61A-Lab 'west s-h)
(can-go s-h 'east 61A-Lab)
(can-go Sproul-Plaza 'east s-h)
(can-go s-h 'west Sproul-Plaza)
(can-go Sproul-Plaza 'north Pimentel)
(can-go Sproul-Plaza 'south Telegraph-Ave)
(can-go Telegraph-Ave 'north Sproul-Plaza)
(can-go Telegraph-Ave 'south Noahs)
(can-go Noahs 'north Telegraph-Ave)
(can-go Noahs 'south Intermezzo)
(can-go Intermezzo 'north Noahs)

;; Some people.
; MOVED above the add-entry-procedure stuff, to avoid the "The computers
; seem to be down" message that would occur when hacker enters 61a-lab
; -- Ryan Stejskal

(define Brian (instantiate person 'Brian BH-Office))
(define hacker (instantiate person 'hacker 61A-lab))
(define nasty (instantiate thief 'nasty sproul-plaza))
(define singer (instantiate person 'rick sproul-plaza))
(ask singer 'set-talk "My funny valentine, sweet comic valentine")
(define preacher (instantiate person 'preacher sproul-plaza))
(ask preacher 'set-talk "Praise the Lord")
(define street-person (instantiate person 'harry telegraph-ave))
(ask street-person 'set-talk "Brother, can you spare a buck")

(define sproul-hall-exit
    (let ((times-called 0))
      (lambda ()
        (set! times-called (+ 1 times-called))
        (if (> times-called 3)
            (print "You can leave now")
		    (error "You can check out any time you'd like, but you can never leave")))))

(define (bh-office-exit)
  (print "What's your favorite programming language?")
  (let ((answer (read)))
    (if (eq? answer 'scheme)
	(print "Good answer, but my favorite is Logo!")
	(begin (newline) (bh-office-exit)))))
    

(ask s-h 'add-entry-procedure
 (lambda () (print "Miles and miles of students are waiting in line...")))
(ask s-h 'add-exit-procedure sproul-hall-exit)
(ask BH-Office 'add-exit-procedure bh-office-exit)
(ask Noahs 'add-entry-procedure
 (lambda () (print "Would you like lox with it?")))
(ask Noahs 'add-exit-procedure
 (lambda () (print "How about a cinnamon raisin bagel for dessert?")))
(ask Telegraph-Ave 'add-entry-procedure
 (lambda () (print "There are tie-dyed shirts as far as you can see...")))
(ask 61A-Lab 'add-entry-procedure
 (lambda () (print "The computers seem to be down")))
(ask 61A-Lab 'add-exit-procedure
 (lambda () (print "The workstations come back to life just in time.")))

;; Some things.

(define coffee (instantiate thing 'coffee))
(ask Intermezzo 'appear coffee)

; 1
(define Dormitory (instantiate place 'Dormitory))
(define Gege (instantiate person 'Gege Dormitory))
;(can-go Dormitory 'east Soda)
;(can-go Soda 'west Dormitory)
;(define Kirin (instantiate place 'Kirin))
;(can-go Soda 'north Kirin)
;(can-go Kirin 'south Soda)
;(define Potstickers (instantiate thing 'Potstickers))
;(ask Kirin 'appear Potstickers)
;(ask Gege 'go 'east)
;(ask Gege 'go 'north)
;(ask Gege 'take Potstickers)
;(ask Gege 'go 'south)
;(ask Gege 'go 'up)
;(ask Gege 'go 'west)
;(ask Gege 'lose Potstickers)
;(ask Brian 'take Potstickers)
;(ask Gege 'go 'east)
;(ask Gege 'go 'down)
;(ask Gege 'go 'south)
;(ask Gege 'go 'south)

; 4
;(define Private-room (instantiate locked-place 'Private-room))
;(can-go Dormitory 'up Private-room)
;(can-go Private-room 'down Dormitory)
;(ask Gege 'go 'up)

; 5
;(define Vehicle (instantiate thing 'vehicle))
;(ask Dormitory 'appear Vehicle)
;(ask Gege 'take Vehicle)
;(define Garage (instantiate Garage))
;(can-go Dormitory 'west Garage)
;(can-go Garage 'east Dormitory)
;(ask Gege 'go 'west)
;(ask Garage 'park Vehicle)
;(define Ticket (car (ask Gege 'possessions)))
;(define Table (ask Garage 'table))
;(ask Garage 'unpark Ticket)

; 5
;(define hs (instantiate hotspot '123))
;(can-go Dormitory 'west hs)
;(can-go hs 'east Dormitory )
;(ask Gege 'go 'west)
;(define laptop (instantiate laptop))
;(ask hs 'appear laptop)
;(ask gege 'take laptop)
;(ask laptop 'connect '123)


;(define yard (instantiate police 'Yard s-h))
;(ask yard 'set-talk "Crime Does Not Pay")
;(ask gege 'buy 'bagel)
;(ask gege 'go 'north)
;(ask gege 'go 'north)