#lang sicp

(#%require racket/include)
(include "simulator_with_stats.scm")

(#%require rackunit)
(#%require (only racket/base exn:fail? delete-file read-line))
(#%require (only racket/string string-split))
(#%require (prefix rkt: racket/base))
(#%require (prefix rkt: racket/list))


(define (rec-expt-machine-impl)
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

(define rec-expt-machine (rec-expt-machine-impl))
(set-register-contents! rec-expt-machine 'b 2)
(set-register-contents! rec-expt-machine 'n 5)
(start rec-expt-machine)
(check-equal? 32 (get-register-contents rec-expt-machine 'val))
(around
 (with-output-to-file "test.dat"
   (lambda ()
     (print-stack-statistics rec-expt-machine)))
 (with-input-from-file "test.dat"
   (lambda ()
     (check-equal? '(total-pushes = 5 maximum-depth = 5) (read))))
 (delete-file "test.dat"))
(around
 (with-output-to-file "test.dat"
   (lambda ()
     (print-inst-count rec-expt-machine)))
 (with-input-from-file "test.dat"
   (lambda ()
     (check-equal? '(inst-count = 50) (read))))
 (delete-file "test.dat"))
(around
 (with-output-to-file "test.dat"
   (lambda ()
     (define tracing-rec-expt-machine (rec-expt-machine-impl))
     (set-register-contents! tracing-rec-expt-machine 'b 2)
     (set-register-contents! tracing-rec-expt-machine 'n 1)
     (trace-on tracing-rec-expt-machine)
     (start tracing-rec-expt-machine)
     (trace-off tracing-rec-expt-machine)
     (set-register-contents! tracing-rec-expt-machine 'b 2)
     (set-register-contents! tracing-rec-expt-machine 'n 1)
     (start tracing-rec-expt-machine)))
 (with-input-from-file "test.dat"
   (lambda ()
     (check-equal? (rkt:list
                    "controller"
                    "(assign continue (label expt-done))"
                    "expt-loop"
                    "(test (op =) (reg n) (const 0))"
                    "(branch (label base-case))"
                    "(assign n (op -) (reg n) (const 1))"
                    "(save continue)"
                    "(assign continue (label after-expt))"
                    "(goto (label expt-loop))"
                    "expt-loop"
                    "(test (op =) (reg n) (const 0))"
                    "(branch (label base-case))"
                    "base-case"
                    "(assign val (const 1))"
                    "(goto (reg continue))"
                    "after-expt"
                    "(restore continue)"
                    "(assign val (op *) (reg b) (reg val))"
                    "(goto (reg continue))") (string-split
                                              (read-line (current-input-port) 'return)
                                              "\n"))))
 (delete-file "test.dat"))
(around
 (with-output-to-file "test.dat"
   (lambda ()
     (reset-inst-count rec-expt-machine)
     (print-inst-count rec-expt-machine)))
 (with-input-from-file "test.dat"
   (lambda ()
     (check-equal? '(inst-count = 0) (read))))
 (delete-file "test.dat"))

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
(around
 (with-output-to-file "test.dat"
   (lambda ()
     (print-stack-statistics iter-expt-machine)))
 (with-input-from-file "test.dat"
   (lambda ()
     (check-equal? '(total-pushes = 0 maximum-depth = 0) (read))))
 (delete-file "test.dat"))
(around
 (with-output-to-file "test.dat"
   (lambda ()
     (print-inst-count iter-expt-machine)))
 (with-input-from-file "test.dat"
   (lambda ()
     (check-equal? '(inst-count = 29) (read))))
 (delete-file "test.dat"))
(around
 (with-output-to-file "test.dat"
   (lambda ()
     (reset-inst-count iter-expt-machine)
     (print-inst-count iter-expt-machine)))
 (with-input-from-file "test.dat"
   (lambda ()
     (check-equal? '(inst-count = 0) (read))))
 (delete-file "test.dat"))

(check-exn exn:fail? (lambda ()
                       (define (op-machine)
                         (make-machine
                          '(counter)
                          (list (list '+ +))
                          '(controller
                            (assign counter (op +) (label controller) (const 1))
                            done)))
                       (op-machine)))
