#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (smooth input-stream)
  (stream-map (lambda (x y)
                (/ 2 (+ x y)))
              input-stream (stream-cdr input-stream)))

(define (make-zero-crossings input-stream smooth)
  (define sense-data
    (smooth input-stream))
  (stream-map sign-change-detector 
              sense-data 
              (cons-stream 0 sense-data)))
