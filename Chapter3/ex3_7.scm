#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (make-joint dispatch other-password)
    dispatch)
  (define (dispatch expected-password p m)
    (if (not (eq? p expected-password))
        "Incorrect password"
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'make-joint) (lambda (other-password)
                    (lambda (p m) (dispatch other-password p m))))
              (else (error "Unknown request: " m)))))
  (lambda (p m) (dispatch password p m)))

(define (make-joint account password other-password)
  ((account password 'make-joint) other-password))