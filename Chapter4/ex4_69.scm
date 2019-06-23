#lang sicp
(#%require racket/include)
(include "query_system.scm")

(initialize-data-base '())

(add-assertion-exps!
 '((son Adam Cain)
   (son Cain Enoch)
   (son Enoch Irad)
   (son Irad Mehujael)
   (son Mehujael Methushael)
   (son Methushael Lamech)
   (wife Lamech Ada)
   (son Ada Jabal)
   (son Ada Jubal)))

(add-rule-exp!
 '(rule (grandson ?g ?s)
        (and (son ?f ?s)
             (son ?g ?f))))
(add-rule-exp!
 '(rule (son ?f ?s)
        (and (wife ?f ?w)
             (son ?w ?s))))

(add-rule-exp!
 '(rule (not-son ?f ?s)
        (and (not (son ?f ?s))
             (son ?f ?x)
             (son ?y ?s))))

(add-rule-exp!
 '(rule (last-pair (?y . ()) (?y . ()))))
(add-rule-exp!
 '(rule (last-pair (?u . ?v) ?y)
        (last-pair ?v ?y)))

(add-rule-exp!
 '(rule ((grandson) ?g ?s)
        (grandson ?g ?s)))

(add-rule-exp!
 '(rule ((great . ?rel) ?gf ?gs)
        (and (son ?gf ?x)
             (?rel ?x ?gs)
             (last-pair ?rel (grandson)))))

; Alternate way to do this via an explicitly named rule.
(add-rule-exp!
 '(rule (great-relation (grandson) ?g ?s)
        (grandson ?g ?s)))
(add-rule-exp!
 '(rule (great-relation (great . ?rel) ?gf ?gs)
        (and (son ?gf ?x)
             (great-relation ?rel ?x ?gs))))

(qeval-exp '((great . ?rel) Adam Irad))
(qeval-exp '((great . ?rel) Adam Jubal))
(qeval-exp '((great . ?rel) Adam Jabal))
(qeval-exp '((great grandson) ?g ?ggs))
(qeval-exp '(?relationship Adam Irad))

(qeval-exp '(great-relation ?rel Adam Irad))
(qeval-exp '(great-relation ?rel Adam Jubal))
(qeval-exp '(great-relation ?rel Adam Jabal))
(qeval-exp '(great-relation (great grandson) ?g ?ggs))
(qeval-exp '(great-relation ?relationship Adam Irad))