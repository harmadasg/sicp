#lang sicp

(define (or-gate a1 a2 output)
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (c1 (make-wire)))
    (inverter a1 b1)
    (inverter a2 b1)
    (and-gate b1 b1 c1)
    (inverter c1 output)
    'ok))