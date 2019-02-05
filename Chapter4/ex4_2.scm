#lang sicp

; 1. Assignment (set! var value) (or for that matter any expression which uses
; a list below the application? clause like define, lambda, etc) will be mistaken
; for application, which just checks for a pair.

; 2.
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
