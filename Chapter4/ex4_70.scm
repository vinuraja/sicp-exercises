#lang sicp

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion 
                     THE-ASSERTIONS))
  'ok)

; In this case, THE-ASSERTIONS will become an infinite stream
; of assertion. This is because cons-stream evaluates its second
; parameter THE-ASSERTIONS in a delayed fashion, and at the evaluation point
; it will 'point' to (cons-stream assertion THE-ASSERTIONS), ending
; up being an infinite stream of 'assertion'.
