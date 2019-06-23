#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base microshaft-data-base)

(add-rule-exp!
 '(rule (outranked-by ?staff-person ?boss)
        (or (supervisor ?staff-person ?boss)
            (and (outranked-by ?middle-manager
                               ?boss)
                 (supervisor ?staff-person 
                             ?middle-manager)))))

; This will get into an infinite loop. The rule-body first tries to
; find if the ?boss is supervisor of ?staff-person, and if not, it will try
; to see if there's are middle-managers in betweeen the ?staff-person and ?boss.
; Unfortunately, (outranked-by ?middle-manager ?boss) will try to see if the
; rule-body has bindings, which goes on to call another (outranked-by ...) and
; so on ad infinitum.
;
; If you switch the order of the sub-clauses in the 'and' clause, then we don't
; enter the infinite loop because (supervisor ?staff-person ?middle-manager) will
; return nothing at some point (when the ?staff-person has no supervisor).
(qeval-exp '(outranked-by (Bitdiddle Ben) ?who))