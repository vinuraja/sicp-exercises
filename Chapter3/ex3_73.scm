#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (RC R C dt)
  (lambda (Si v0)
    (add-streams (scale-stream Si R)
                 (integral (scale-stream Si (/ 1 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))

