#lang sicp

; One possibility is to store the rule applications in a list. A rule
; application would consist of the rule-name and the variable bindings
; as well. If the rule which is going to be applied matches any of the
; rules which are being currently applied, in the list, then we have
; an infinite loop. A match is made when the rule-name and the unbound
; variables and bound-variables match.