#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base microshaft-data-base)

(qeval-exp '(lives-near ?person (Hacker Alyssa P)))

; If this will satisfy assignments ?person-1=A and ?person-2=B,
; then it will also satisfy ?person-1=B and ?person-1=A. Hence
; we see repetitions of that form.
(qeval-exp '(lives-near ?person-1 ?person-2))

; This can be fixed by giving some particular order to the
; assignments of ?person-1 and ?person-2. For example, lexical
; ordering.
; Note that this will cause
; (lives-near-ordered ?person-1 (Hacker Alyssa P))
; to only output folks who live near Alyssa, but have their
; names ordered 'lesser' than them. To fix this, lives-near
; could be changed to:
; (rule (lives-near-person ?person)
;       (or (lives-near-ordered ?person ?other)
;           (lives-near-ordered ?other ?person)))

(#%require (prefix rkt: racket/base))

(add-rule-exp!
 '(rule (lives-near-ordered ?person-1 ?person-2)
        (and (address ?person-1 
                      (?town . ?rest-1))
             (address ?person-2 
                      (?town . ?rest-2))
             (lisp-value (lambda (p1 p2)
                           (string<? (symbol->string (car p1))
                                     (symbol->string (car p2))))
                         ?person-1 ?person-2)
             (not (same ?person-1 ?person-2)))))

(qeval-exp '(lives-near-ordered ?person-1 ?person-2))