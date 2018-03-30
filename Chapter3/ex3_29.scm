#lang sicp
(define (or-gate a1 a2 output)
  (let ((a1not (make-wire))
        (a2not (make-wire))
        (a1notanda2not (make-wire)))
    (inverter a1 a1not)
    (inverter a2 a2not)
    (and-gate a1not a2not a1notanda2not)
    (inverter a1notanda2not output)
    'ok))

; or-gate-delay = 2 * inverter-delay + and-gate-delay