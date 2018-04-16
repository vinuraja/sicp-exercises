#lang sicp
(#%require racket/include)
(include "streams.scm")


(define (simple-pairs s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (simple-pairs (stream-cdr s)
                 (stream-cdr t))))

; This will cause infinite recursion because 'simple-pairs'
; doesn't have delayed evaluation in the 'interleave' method
; so 'simple-pairs will be called again and again.
; (define int-simple-pairs (simple-pairs integers integers))