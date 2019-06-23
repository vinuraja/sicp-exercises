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

(qeval-exp '(not-son ?f ?s))
(qeval-exp '(grandson Cain ?s))
(qeval-exp '(son Lamech ?s))
(qeval-exp '(grandson Methushael ?s))
