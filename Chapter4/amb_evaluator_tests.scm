#lang sicp
(#%require rackunit)

(#%require racket/include)
(include "amb_evaluator.scm")

(define (eval exp env)
  (ambeval exp env
           ;; succeed
           (lambda (val fail)
             val)
           ;; fail
           (lambda () 'failed)))

; Runs ambeval in a loop, and outputs all the answers
; as a list. If n > 0, only runs ambeval atmost n times,
; thereby limiting the answers to atmost n..
(define (ambeval-loop-until exp env n)
  (define count n)
  (define (internal-loop first? try-next)
    (cond ((= count 0) '())
          (first? (ambeval exp env
                           (lambda (val next-alternative)
                             (set! count (- count 1))
                             (cons val
                                   (internal-loop #f next-alternative)))
                           (lambda () '())))
          (else (try-next))))
  (internal-loop #t (lambda () '())))
(define (ambeval-loop exp env)
  (ambeval-loop-until exp env -1))

(check-equal? 3 (eval (+ 1 2) genv))
(check-equal? '(+ 1 2) (eval ''(+ 1 2) genv))
(check-equal? 3 (eval '(+ 1 2) genv))

(check-equal? 5 (eval '(cond (1 (+ 2 3)) (else false)) genv))
(check-equal? 2 (eval '(cond ('(b 2) => cadr) (else false)) genv))

(check-equal? 3 (eval '((lambda (x) (+ 1 x)) 2) genv))
(check-equal? 3 (eval '((lambda (x y) (+ y x)) 1 2) genv))
(check-equal? 3 (eval '(let ((x 2)) (+ 1 x)) genv))
(check-equal? 3 (eval '(let ((x 2) (y 1))
                         (= x 3) ; verifies multiple statement support
                         (+ y x)) genv))

(check-equal? 8 (eval '((lambda (n)
                          (let fib-iter ((a 1) (b 0) (count n))
                            (if (= count 0)
                                b
                                (fib-iter (+ a b) 
                                          a 
                                          (- count 1)))))
                        6)
                      genv))
(check-equal? 8 (eval '((lambda (n)
                          (let fib-iter ((a 1) (b 0) (count n))
                            (= a 0) ; verifies multiple statement support
                            (if (= count 0)
                                b
                                (fib-iter (+ a b) 
                                          a 
                                          (- count 1)))))
                        6)
                      genv))
(check-equal? 'ok (eval '(define (append x y)
                           (if (null? x)
                               y
                               (cons (car x) (append (cdr x) y)))) genv))
(check-equal? '(a b c d e f) (eval '(append '(a b c) '(d e f)) genv))
(check-equal? 'ok (eval '(define (map f x)
                           (if (null? x)
                               '()
                               (cons (f (car x)) (map f (cdr x))))) genv))
(check-equal? '(2 4 6 8) (eval '(map (lambda (x) (* 2 x)) '(1 2 3 4)) genv))
(check-equal? 'ok (eval '(define (map-even? x)
                           (define (even? n)
                             (if (= n 0)
                                 true
                                 (odd? (- n 1))))
                           (define (odd? n)
                             (if (= n 0)
                                 false
                                 (even? (- n 1))))
                           (map even? x)) genv))
(check-equal? '(#f #t #f #t) (eval '(map-even? '(1 2 3 4)) genv))

(check-equal? 'ok (eval '(define (require p)
                           (if (not p) (amb))) genv))
(check-equal? 'ok (eval '(define (an-integer-between low high)
                           (require (<= low high))
                           (amb low (an-integer-between (+ low 1) high))) genv))

(check-equal? '(1) (ambeval-loop '(an-integer-between 1 1) genv))
(check-equal? '(1 2 3 4 5) (ambeval-loop '(an-integer-between 1 5) genv))
(check-equal? '(1 2 3 4 5) (ambeval-loop-until '(an-integer-between 1 5) genv 6))
(check-equal? '(1 2) (ambeval-loop-until '(an-integer-between 1 5) genv 2))
(check-equal? '() (ambeval-loop-until '(an-integer-between 1 5) genv 0))
(check-equal? '() (ambeval-loop '(an-integer-between 1 0) genv))
(check-equal? '() (ambeval-loop-until '(an-integer-between 1 0) genv 1))
(check-equal? '() (ambeval-loop-until '(an-integer-between 1 0) genv 0))