#lang sicp

(#%require racket/include)
(include "simulator_with_stats.scm")

(#%require rackunit)
(#%require (only racket/base exn:fail?))

(define (rec-fact-machine-impl)
  (make-machine
   '(n val continue)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
     (assign continue (label fact-done))   ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)                       ; Set up for the recursive call
     (save n)                              ; by saving n and continue.
     (assign n (op -) (reg n) (const 1))   ; Set up continue so that the
     (assign continue (label after-fact))  ; computation will continue
     (goto (label fact-loop))              ; at after-fact when the
     after-fact                              ; subroutine returns.
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ; val now contains n(n - 1)!
     (goto (reg continue))                 ; return to caller
     base-case
     (assign val (const 1))                ; base case: 1! = 1
     (goto (reg continue))                 ; return to caller
     fact-done)))

(define rec-fact-machine (rec-fact-machine-impl))
(set-register-contents! rec-fact-machine 'n 3)
(start rec-fact-machine)
(check-equal? 6 (get-register-contents rec-fact-machine 'val))
(print-stack-statistics rec-fact-machine)  ; (total-pushes = 4 maximum-depth = 4)

(set! rec-fact-machine (rec-fact-machine-impl))
(set-register-contents! rec-fact-machine 'n 4)
(start rec-fact-machine)
(check-equal? 24 (get-register-contents rec-fact-machine 'val))
(print-stack-statistics rec-fact-machine)  ; (total-pushes = 6 maximum-depth = 6)

(set! rec-fact-machine (rec-fact-machine-impl))
(set-register-contents! rec-fact-machine 'n 5)
(start rec-fact-machine)
(check-equal? 120 (get-register-contents rec-fact-machine 'val))
(print-stack-statistics rec-fact-machine)  ; (total-pushes = 8 maximum-depth = 8)

; So maximum-depth = total-pushes = 2 * n - 2