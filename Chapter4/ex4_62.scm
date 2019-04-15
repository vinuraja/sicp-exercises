#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base '())

(add-rule-exp!
 '(rule (last-pair ?y ?y)
        (lisp-value (lambda (lst)
                      (= (length lst) 1))
                    ?y)))
(add-rule-exp!
 '(rule (last-pair (?u . ?v) ?y)
        (last-pair ?v ?y)))

(qeval-exp '(last-pair (3) ?x))
(qeval-exp '(last-pair (1 2 3) ?x))
(qeval-exp '(last-pair (2 ?x) (3)))
; This will run infinitely, because the answers are any lists
; ending with 3.
;(qeval-exp '(last-pair ?x (3)))