;#lang sicp

(#%require rackunit)
(#%require (only racket/base exn:fail?))

(test-begin
  (define env0 (extend-environment '(v) '(1) the-empty-environment))
  (define env1 (extend-environment '(var) '(2) env0))
  (check-exn exn:fail? (lambda () (lookup-variable-value 'var env0)))
  (check-exn exn:fail? (lambda () (set-variable-value! 'var 0 env0)))
  (define-variable! 'var 0 env0)
  (check-equal? (lookup-variable-value 'var env0) 0)
  (set-variable-value! 'var 1 env0)
  (check-equal? (lookup-variable-value 'var env0) 1)
  (check-equal? (lookup-variable-value 'v env1) 1)
  (check-equal? (lookup-variable-value 'var env1) 2)
  (define-variable! 'var 3 env1)
  (check-equal? (lookup-variable-value 'var env1) 3))