#lang sicp
(#%require racket/include)
(include "evaluator.scm")

(define (eval exp env)
  (vanilla-eval exp env))

(define (timed-vanilla-eval exp env)
  (time (vanilla-eval exp env)))

(timed-vanilla-eval '(define fib ((lambda (n)
          (let fib-iter ((a 1) (b 0) (count n))
            (= a 0) ; verifies multiple statement support
            (if (= count 0)
                b
                (fib-iter (+ a b) 
                          a 
                          (- count 1)))))
        4000))
      genv)

(define (timed-analyze-eval exp env)
  (time (analyze-eval exp env)))

(timed-analyze-eval '(define fib ((lambda (n)
          (let fib-iter ((a 1) (b 0) (count n))
            (= a 0) ; verifies multiple statement support
            (if (= count 0)
                b
                (fib-iter (+ a b) 
                          a 
                          (- count 1)))))
        4000))
      genv)

; On average, timed-vanilla-eval takes around 135ms and timed-analyze-eval
; takes around 65ms. The main time difference would be in re-analyzing the
; fib-iter method again and again, around 4000 times, in the case of the
; former. So on average, analyzing fib-iter is taking (135 - 65)/4000 = 0.0175ms
; (or 17.5us). And pure execution is 65ms.
