#lang sicp
; If flatten-stream is implemented without delay, it will
; essentially try to flatten all the streams at once, without
; lazily evaluating each stream. So if there are any infinite
; streams, we will be stuck in those without any answers.

; For example:
(#%require racket/include)
(include "query_system.scm")

(initialize-data-base '())

(add-assertion-exps!
 '((ones ())
   (twos ())))

(add-rule-exp!
 '(rule (ones (1 . ?x)) (ones ?x)))
(add-rule-exp!
 '(rule (twos (2 . ?x)) (twos ?x)))

(add-rule-exp!
 '(rule (ones-and-twos ?x ?y)
        (and (ones ?x) (twos ?y))))

; With interleave it will print both 1s and 2s. Otherwise it will
; print just 1s.
(qeval-exp '(ones-and-twos ?x ?y))