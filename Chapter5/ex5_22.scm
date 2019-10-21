#lang sicp

(#%require racket/include)
(include "simulator_with_stats.scm")

(#%require rackunit)
(#%require (only racket/base exn:fail?))

(define (append-machine-impl)
  (make-machine
   '(x y val continue)
   (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'cons cons))
   '(controller
     (assign continue (label append-done))
     append-recn
     (test (op null?) (reg x))
     (branch (label null-x-case))
     (save x)
     (assign x (op cdr) (reg x))
     (save continue)
     (assign continue (label after-append))
     (goto (label append-recn))
     after-append
     (restore continue)
     (restore x)
     (assign x (op car) (reg x))
     (assign val (op cons) (reg x) (reg val))
     (goto (reg continue))
     null-x-case
     (assign val (reg y))
     (goto (reg continue))
     append-done)))

(define append-machine0 (append-machine-impl))
(set-register-contents! append-machine0 'x '())
(set-register-contents! append-machine0 'y '(3 4 5))
; For debugging
; (trace-on append-machine0)
(start append-machine0)
(check-equal? '(3 4 5) (get-register-contents append-machine0 'val))

(define append-machine1 (append-machine-impl))
(set-register-contents! append-machine1 'x '(1 2))
(set-register-contents! append-machine1 'y '())
(start append-machine1)
(check-equal? '(1 2) (get-register-contents append-machine1 'val))

(define append-machine2 (append-machine-impl))
(set-register-contents! append-machine2 'x '(1 2))
(set-register-contents! append-machine2 'y '(3 4 5))
(start append-machine2)
(check-equal? '(1 2 3 4 5) (get-register-contents append-machine2 'val))

(define (append!-machine-impl)
  (make-machine
   '(x y val cdr-x)
   (list (list 'null? null?) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
   '(controller
     (save x)
     last-pair-recn
     (assign cdr-x (op cdr) (reg x))
     (test (op null?) (reg cdr-x))
     (branch (label null-x-case))
     (assign x (op cdr) (reg x))
     (goto (label last-pair-recn))
     null-x-case
     (assign val (reg x))
     last-pair-done
     (restore x)
     (perform (op set-cdr!) (reg val) (reg y))
     (assign val (reg x))
     append!-done)))

(define append!-machine0 (append!-machine-impl))
(set-register-contents! append!-machine0 'x '())
(set-register-contents! append!-machine0 'y '(3 4 5))
; For debugging
; (trace-on append!-machine0)
; Failure expected when called with empty x.
(check-exn exn:fail? (lambda()
                       (start append!-machine0)))

(define append!-machine1 (append!-machine-impl))
(set-register-contents! append!-machine1 'x '(1 2))
(set-register-contents! append!-machine1 'y '())
; (trace-on append!-machine1)
(start append!-machine1)
(check-equal? '(1 2) (get-register-contents append!-machine1 'val))

(define append!-machine2 (append!-machine-impl))
(set-register-contents! append!-machine2 'x '(1 2))
(set-register-contents! append!-machine2 'y '(3 4 5))
(start append!-machine2)
(check-equal? '(1 2 3 4 5) (get-register-contents append!-machine2 'val))