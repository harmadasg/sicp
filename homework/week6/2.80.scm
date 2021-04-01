#lang simply-scheme

(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  ;; interface to the rest of the system
  ;...
  (put '=zero? '(scheme-number) zero?)
  'done)

(define (install-rational-package)
  ;; internal procedures
  ;...
  (define (=zero?-rat x) (= (numer x) 0))
  ;; interface to the rest of the system
  ;...
  (put '=zero? '(rational) =zero?-rat)
  'done)

(define (install-complex-package)
  ;; internal procedures
  ;...
  (define (=zero?-comp x) (and (= (real-part x) 0) (= (imag-part x) 0)))
  ;; interface to the rest of the system
  ;...
  (put '=zero? '(complex) =zero?-comp)
  'done)
