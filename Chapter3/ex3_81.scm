#lang sicp
(#%require racket/include)
(include "streams.scm")

; Requests will be of the form 'generate or ('reset <value>)
(define random-init 9)
(define (rand-update x) (+ x 1))
(define (random-numbers requests)
  (define random-stream
    (cons-stream random-init
                 (stream-map (lambda (n r)
                               (cond ((eq? r 'generate) (rand-update n))
                                     ((and (pair? r) (eq? (car r) 'reset)) (cadr r))
                                     (else (error "Incorrect request format!" r))))
                             random-stream requests)))
  random-stream)

(define rnumbers
  (random-numbers (cons-stream
                   'generate
                   (cons-stream
                    '(reset 15)
                    (cons-stream
                     'generate
                     (cons-stream
                      'generate
                      (cons-stream '(reset 51) the-empty-stream)))))))
(display-stream rnumbers)