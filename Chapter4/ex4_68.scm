#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base '())

(add-rule-exp!
 '(rule (append-to-form () ?y ?y)))
(add-rule-exp!
 '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
        (append-to-form ?v ?y ?z)))
(add-rule-exp!
 '(rule (reverse (?u . ()) (?u))))
(add-rule-exp!
 '(rule (reverse (?u . ?v) ?y)
        (and (reverse ?v ?rev-v)
             (append-to-form ?rev-v (?u) ?y))))

(add-rule-exp!
 '(rule (reverse-other ?x ?y)
        (reverse ?y ?x)))
; This will get into an infinite loop, because either reverse or reverse-other
; will get into an infinite loop.
(add-rule-exp!
 '(rule (reverse-me ?x ?y)
        (or (reverse ?x ?y) (reverse-other ?x ?y))))

; The reverse rule implemented here can have the unbound variable only
; as the second param. If the first param is set unbound, we will go
; into an infinite loop. This is because we will try to solve (reverse (?u . ?v) ?y)
; where (?u . ?v) is unbound, and hence (reverse ?v ?rev-v) will get
; into an infinite loop.

; If you want to use the unbound variable as the first param, you can use
; reverse-other. But it's not that useful since the caller can reverse
; the params themselves. If the query language had support for figuring out which
; param was unbound, and then using the right rule, then it would have been
; useful here.

(qeval-exp '(reverse () ?x))
(qeval-exp '(reverse-other ?x ()))
(qeval-exp '(reverse (1) ?x))
(qeval-exp '(reverse-other ?x (1)))
(qeval-exp '(reverse (1 2) ?x))
(qeval-exp '(reverse (2 3) ?x))
(qeval-exp '(reverse (1 2 3) ?x))
(qeval-exp '(reverse-other ?x (1 2 3)))