; make-unbound! would unbind the variable only in the enclosing
; frame, its own scratch space. Otherwise, it will be able to
; hard for the rest of the program to keep track of what's happening
; in it's own frame.

#lang sicp

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (zip vars vals)
  (if (= (length vars) (length vals))
      (if (null? vars)
          '()
          (cons (cons (car vars) (car vals)) (zip (cdr vars) (cdr vals))))
      (error "Unequal number of vars and vals supplied" 
                 vars 
                 vals)))

(define (make-frame variables values)
  (zip variables values))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (get-frame-variable-var frame)
  (caar frame))
(define (get-frame-variable-val frame)
  (cdar frame))
(define (set-frame-variable-val! frame val)
  (set-cdr! (car frame) val))

(define (scan-frame frame var)
  (if (or (null? frame) (eq? var (get-frame-variable-var frame)))
      frame
      (scan-frame (cdr frame) var)))
  
(define (scan-frame-in-env env var)
  (let ((frame (first-frame env)))
    (if (eq? env the-empty-environment)
        '()
        (let ((found-frame (scan-frame frame var)))
          (if (null? found-frame)
              (scan-frame-in-env (enclosing-environment env) var)
              found-frame)))))
  
(define (lookup-variable-value var env)
  (let ((found-frame (scan-frame-in-env env var)))
    (if (null? found-frame)
        (error "Unbound variable" var env)
        (get-frame-variable-val found-frame))))

(define (set-variable-value! var val env)
  (let ((found-frame (scan-frame-in-env env var)))
    (if (null? found-frame)
        (error "Unbound variable: SET!" var env)
        (set-frame-variable-val! found-frame val))))

(define (define-variable! var val env)
  (let ((found-frame (scan-frame (first-frame env) var)))
    (if (null? found-frame)
        (add-binding-to-frame! var val (first-frame env))
        (set-frame-variable-val! found-frame val))))

(define (make-unbound! var env)
  (define (scan-frame-with-prev frame var prev)
    (if (or (null? frame) (eq? var (get-frame-variable-var frame)))
        (cons prev frame)
        (scan-frame-with-prev (cdr frame) var frame)))
  (let ((prev-and-found-frame (scan-frame-with-prev (first-frame env) var '())))
    (let ((prev (car prev-and-found-frame))
          (found-frame (cdr prev-and-found-frame)))
      (cond ((null? found-frame)
             (error "Already unbound variable" var env))
            ((null? prev) (set-car! env '()))
            (else (set-cdr! prev (cdr found-frame)))))))

(#%require racket/include)
(include "environment_tests.scm")
(test-begin
  (define env0 (extend-environment '(var) '(0) the-empty-environment))
  (define env1 (extend-environment '(var) '(1) env0))
  (define env2 (extend-environment '(var) '(2) env1))
  (check-equal? (lookup-variable-value 'var env2) 2)
  (make-unbound! 'var env2)
  (check-exn exn:fail? (lambda () (make-unbound! 'var env2)))
  (check-equal? (lookup-variable-value 'var env2) 1)
  (make-unbound! 'var env1)
  (check-exn exn:fail? (lambda () (make-unbound! 'var env1)))
  (check-equal? (lookup-variable-value 'var env1) 0)
  (make-unbound! 'var env0)
  (check-exn exn:fail? (lambda () (make-unbound! 'var env0)))
  (check-exn exn:fail? (lambda () (lookup-variable-value 'var env0))))
  