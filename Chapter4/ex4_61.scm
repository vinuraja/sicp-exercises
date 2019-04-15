#lang sicp
(#%require racket/include)
(include "query_system.scm")

(initialize-data-base '())

(add-rule-exp!
 '(rule (?x next-to ?y in (?x ?y . ?u))))
(add-rule-exp!
 '(rule (?x next-to ?y in (?v . ?z))
        (?x next-to ?y in ?z)))

; This should give answers:
; 1 next-to (2 3)
; (2 3) next-to 4
(qeval-exp '(?x next-to ?y in (1 (2 3) 4)))

; 2 next-to 1
; 3 next-to 1
(qeval-exp '(?x next-to 1 in (2 1 3 1)))