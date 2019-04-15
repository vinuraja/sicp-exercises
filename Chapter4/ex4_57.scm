#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base microshaft-data-base)

(add-rule-exp!
 '(rule (replace ?p1 ?p2)
        (and (job ?p1 ?jb1)
             (job ?p2 ?jb2)
             (or (same ?jb1 ?jb2)
                 (can-do-job ?jb1 ?jb2))
             (not (same ?p1 ?p2)))))

; 1.
(qeval-exp '(replace ?x (Fect Cy D)))

; 2.
(qeval-exp
 '(and (replace ?p ?rep)
       (salary ?p ?psal)
       (salary ?rep ?repsal)
       (lisp-value > ?repsal ?psal)))