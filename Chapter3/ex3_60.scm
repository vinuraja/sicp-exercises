#lang sicp
(#%require racket/include)
(include "streams.scm")

; 1 + 0 + 0 ...
(define one (add-streams
             (mul-series sine-series sine-series)
             (mul-series cosine-series cosine-series)))