#lang simply-scheme

(define (equ x y) (apply-generic 'equ x y))

(define (install-scheme-number-package)
  ;; interface to the rest of the system
  ;...
  (put 'equ '(scheme-number scheme-number) =)
  'done)

(define (install-rational-package)
  ;; internal procedures
  ;...
  (define (equ-rat x y) (= (/ (numer x) (denom x)) (/ (numer y) (denom y))))
  ;; interface to the rest of the system
  ;...
  (put 'equ '(rational rational) equ-rat)
  'done)

(define (install-complex-package)
  ;; internal procedures
  ;...
  (define ( equ-complex x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  ;; interface to the rest of the system
  ;...
  (put 'equ '(complex complex) equ-complex)
  'done)
