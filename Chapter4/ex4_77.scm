#lang sicp

; A sketch for the solution:
;
; You can basically use store promises inside the frame. A promise
; will consist of (variables, fn) where variables are the set
; of variables that need to be bound, and fn is the function which
; will be run when all these variables are found to be bound. We
; can check for bound variables and execute the promise when we
; extend the frame with a (var, val). If the execution is a failure, we
; can return 'failed, and this will work out in the already written
; query_system.scm.
