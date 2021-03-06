#lang sicp

(#%require racket/include)
(include "simulator.scm")

(#%require rackunit)
(#%require (only racket/base exn:fail?))

(define rec-expt-machine
  (make-machine
   '(b n val continue)
   (list (list '- -) (list '= =) (list '* *))
   '(controller
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (assign n (op -) (reg n) (const 1))
     (save continue)
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

(set-register-contents! rec-expt-machine 'b 2)
(set-register-contents! rec-expt-machine 'n 5)
(start rec-expt-machine)
(check-equal? 32 (get-register-contents rec-expt-machine 'val))

(define iter-expt-machine
  (make-machine
   '(b n counter product)
   (list (list 'sub -) (list '= =) (list 'mul *))
   '(controller
     (assign counter (reg n))
     (assign product (const 1))
     test-counter
     (test (op =) (reg counter) (const 0))
     (branch (label expt-done))
     (assign product (op mul) (reg b) (reg product))
     (assign counter (op sub) (reg counter) (const 1))
     (goto (label test-counter))
     expt-done)))
 
(set-register-contents! iter-expt-machine 'b 2)
(set-register-contents! iter-expt-machine 'n 5)
(start iter-expt-machine)
(check-equal? 32 (get-register-contents iter-expt-machine 'product))

(check-exn exn:fail? (lambda ()
                       (define (op-machine)
                         (make-machine
                          '(counter)
                          (list (list '+ +))
                          '(controller
                            (assign counter (op +) (label controller) (const 1))
                            done)))
                       (op-machine)))
