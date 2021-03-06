#lang sicp
(#%require racket/include)
(include "constraint.scm")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (divider a b c)
  (define (process-new-value)
    (cond ((and (has-value? a)
                (has-value? b))
           (set-value! c (/ (get-value a) (get-value b)) me))
          ((and (has-value? a)
                (has-value? c))
           (set-value! b (/ (get-value a) (get-value c)) me))
          ((and (has-value? b)
                (has-value? c))
           (set-value! a (* (get-value b) (get-value c)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        DIVIDER" request))))
  (connect a me)
  (connect b me)
  (connect c me)
  me)

(define (c/ x y)
  (let ((z (make-connector)))
    (divider x y z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9.0) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe "C" C)
(probe "F" F)
