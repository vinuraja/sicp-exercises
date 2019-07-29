#lang sicp

; Without the interleave, you could get into certain ruts while
; doing the matching, while interleave lets you get out of those
; ruts by trying other paths as well.

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
 '(rule (ones-or-twos ?x ?y)
        (or (ones ?x) (twos ?y))))

; With interleave it will print both 1s and 2s. Otherwise it will
; print just 1s.
(qeval-exp '(ones-or-twos ?x ?y))