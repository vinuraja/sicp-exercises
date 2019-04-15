#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base microshaft-data-base)

(add-assertion-exps!
 '((meeting accounting (Monday 9am))
   (meeting administration (Monday 10am))
   (meeting computer (Wednesday 3pm))
   (meeting administration (Friday 1pm))
   (meeting whole-company (Wednesday 4pm))))

; 1.
(qeval-exp '(meeting ?div (Friday ?time)))

; 2.
(add-rule-exp!
 '(rule (meeting-time ?person ?day-and-time)
        (or (meeting whole-company ?day-and-time)
            (and (job ?person (?div . ?type))
                 (meeting ?div ?day-and-time)))))

; 3.
(qeval-exp '(meeting-time (Hacker Alyssa P) (Wednesday ?time)))