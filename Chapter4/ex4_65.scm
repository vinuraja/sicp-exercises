#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base microshaft-data-base)

; The following rule declares that a person is a “wheel” in an organization
; if he supervises someone who is in turn a supervisor:
;
; (rule (wheel ?person)
;       (and (supervisor ?middle-manager 
;                        ?person)
;            (supervisor ?x ?middle-manager)))

; Warbucks Oliver shows up 4 times, because there are 4 folks for whom he is a
; skip-manager: 1 through (Scrooge Eben) and 3 through (Bitdiddle Ben).