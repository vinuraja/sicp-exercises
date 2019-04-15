#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base '())

(add-rule-exp!
 '(rule (append-to-form () ?y ?y)))
(add-rule-exp!
 '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
        (append-to-form ?v ?y ?z)))

(qeval-exp '(append-to-form (a b) (c d) ?z))
(qeval-exp '(append-to-form (a b) ?y (a b c d)))
(qeval-exp '(append-to-form ?x ?y (a b c d)))