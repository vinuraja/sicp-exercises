#lang sicp
(#%require racket/include)
(include "streams.scm")

; Added mul-streams to streams.scm

(define factorials 
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))