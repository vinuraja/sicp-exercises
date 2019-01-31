#lang sicp
; I can definitely understand where Alyssa is coming from w.r.t.
; treating methods and variables the same way for
; consistency. I think I disagree with Eva and Ben though.
; You can get the behaviour that Eva wants by converting the defined
; variables to lambdas. So instead of (define b (+ a 5)), we'll
; have (define (b) (+ (a) 5)), and change usages of b to (b), and
; apply the same transformation as in ex4.16. Since a and b are now
; lambdas, which are set later, we will get simultaneous scoping,
; where (a) will be 5, and (b) 15.