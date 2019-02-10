#lang sicp
(#%require racket/include)
(include "lazy_evaluator.scm")
(include "lazy_evaluator_tests.scm")

; 1.
(eval '(define (for-each proc items)
         (if (null? items)
             'done
             (begin (proc (car items))
                    (for-each proc 
                              (cdr items))))) genv)

(eval '(for-each
        (lambda (x) (newline) (display x))
        (list 57 321 88)) genv)

; In the case of this example things work because newline and display are
; primitive procedures are not thunked.

; 2.
(eval '(define (p1 x)
         (set! x (cons x '(2))) x) genv)
(check-equal? (force-it (eval '(p1 1) genv)) '(1 2))

(eval '(define (p2 x)
  (define (p e) e x)
  (p (set! x (cons x '(2))))) genv)

; Before Cy's change: (check-equal? (force-it (eval '(p2 1) genv)) 1)
; After
(check-equal? (force-it (eval '(p2 1) genv)) '(1 2))

; 3.
; Cy's change doesn't affect 1. because the form in 1. uses primitive
; procedure newline, which will be strictly evaluated.

; 4.
; Out of the 2 approaches, I think Cy's approach is correct, compared to
; the approach of the text, if the language allows side-effects and isn't
; purely functional.

; Now, if side-effects were just contained to assignments, we could maybe
; have a new way of doing things. Variables will have a list of values, with
; values at the beginning being the current ones, and values could be thunks.
; Assignment expressions will be delayed by adding a thunk at the beginning of
; this list. So now, when a variable's value is forced, we can go through this
; list and force the current value. This is more lazy than the current approach,
; because set! will have an effect, without requiring forcing of the whole value.