#lang racket

; The current vanilla eval evaluates the if condition lazily,
; ie, the alternative is evaluated only if the predicate is false,
; and similarly the consequent is evaluated only if the predicate is
; true. So we can convert unless to if.

(#%require racket/include)
(#%require rackunit)
(#%require (only racket/base foldr))

(include "evaluator.scm")

(define (install-unless-package)
  (define (eval-unless exp env)
    (eval (unless->if exp) env))
  (put 'eval 'unless eval-unless))
(install-unless-package)

(define (unless->if exp)
  (list 'if (cadr exp) (cadddr exp) (caddr exp)))

(define (eval exp env)
  (vanilla-eval exp env))

(eval '(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1)) genv)

(check-equal? (eval '(factorial 5) genv) 120)

; I couldn't find a good reason to have unless be a procedure rather
; than a syntax sugar. I know that this precludes this from being used
; in higher-order functions, but I couldn't find a good use of unless
; in such a case, where laziness is also necessarily required. I could
; find an example online of the use, but again the laziness is not a
; a requirement here.
; (define select-y '(#t #f #t #t)) 
; (define xs '(1 3 5 7)) 
; (define ys '(2 4 6 8)) 
; (define selected (map unless select-y xs ys))