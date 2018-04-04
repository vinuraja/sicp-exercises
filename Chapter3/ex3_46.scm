#lang sicp
; If test-and-set! isn't made atomic, it could potentially let
; 2 process acquire the same mutex concurrently, thereby not
; guaranteeing serialization, and lead to data-race issues like
; an account withdrawal overwriting another.
;
; The mutex would be acquired at the same time because test-and-set!
; could potentially return false to 2 processes, if both of them are
; allowed to test the value at the same time.