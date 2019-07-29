#lang sicp

; Having the explicit delay would mean the argument is evaluated
; lazily. This would help in preventing any infinite recursion
; issues until the last moment possible.
; For example:
; (assert! (married? Mickey Minnie))
; (assert! (rule (married ?x ?y) 
;                (married ?y ?x)))
