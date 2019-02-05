#lang sicp

(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

; If halts? can be implemented, it means that (try try) will do
; the inverse of (halts? try try) ie, if (halts try try) is true,
; it runs forever, otherwise halts. This is contradictory as
; (halts? try try) is supposed to describe the exact halting
; behaviour of (try try).