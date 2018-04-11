#lang sicp
(#%require racket/include)
(include "streams.scm")

; Added merge to streams.scm

(define S
  (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))
