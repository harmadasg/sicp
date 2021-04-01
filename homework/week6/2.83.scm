#lang simply-scheme

(define (raise x) (apply-generic 'raise x))

;; let scheme-number be integer for convenience
(define (install-scheme-number-package)
  ;; internal procedures
  ;...
  (define (scheme-number->rational x) (make-rat x 1))
  ;; interface to the rest of the system
  ;...
  (put 'raise '(scheme-number) scheme-number->rational)
  'done)

(define (install-rational-package)
  ;; internal procedures
  ;...
  (define (rational->real x) (/ (numer x) (denom x)))
  ;; interface to the rest of the system
  ;...
  (put 'raise 'rational '(real) rational->real)
  'done)

(define (install-real-package)
  ;; internal procedures
  ;...
  (define (real->complex x) (make-complex-from-real-imag x 0))
  ;; interface to the rest of the system
  ;...
  (put 'raise '(real)  real->complex)
  'done)
