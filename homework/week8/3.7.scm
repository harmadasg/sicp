#lang simply-scheme

(define (make-account balance password)
  (let ((passwords (list password)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (add-pw pw) (set! passwords (cons pw passwords)) passwords)
    (define (dispatch pw m)
      (cond ((not (member? pw passwords)) (error "Incorrect password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'joint) add-pw)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (make-joint acc acc-password new-password)
  ((acc acc-password 'joint) new-password) acc)