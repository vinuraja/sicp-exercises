#lang sicp

(#%require racket/include)
(include "amb.scm")

(define (while pred body prev)
  (if (pred prev)
      (while pred body (body prev))
      prev))

(define (satisfies? baker cooper fletcher miller smith)
  (and (distinct? (list baker cooper fletcher 
                        miller smith))
       (not (= (abs (- smith fletcher)) 1))
       (not (= (abs (- fletcher cooper)) 1))
       (> miller cooper)
       (not (= fletcher 1))
       (not (= fletcher 5))
       (not (= cooper 1))
       (not (= baker 5))))

; We basically use 5 loops with each going up to 5, and in the innermost loop,
; check whether the conditions for a solution are satisfied.
; If you feel this is not elegant, another solution is to find the permutations
; of '(1 2 3 4 5) (every number is distinct :)), and then try satisfies? (without
; distinct?) against that.
(define (multiple-dwelling)
  (cdr
   (while
    (lambda (b) (<= (car b) 5))
    (lambda (b)
      (let* ((baker (car b))
             (bresults
              (while
               (lambda (c) (<= (car c) 5))
               (lambda (c)
                 (let* ((cooper (car c))
                        (cresults
                         (while
                          (lambda (f) (<= (car f) 5))
                          (lambda (f)
                            (let* ((fletcher (car f))
                                   (fresults
                                    (while
                                     (lambda (m) (<= (car m) 5))
                                     (lambda (m)
                                       (let* ((miller (car m))
                                              (mresults
                                               (while
                                                (lambda (s) (<= (car s) 5))
                                                (lambda (s)
                                                  (let* ((smith (car s)))
                                                    (cons (+ smith 1)
                                                          (append
                                                           (cdr s)
                                                           (if (satisfies?
                                                                baker cooper fletcher miller smith)
                                                               (list (list 'baker baker)
                                                                     (list 'cooper cooper)
                                                                     (list 'fletcher fletcher)
                                                                     (list 'miller miller)
                                                                     (list 'smith smith))
                                                               nil)))))
                                                (cons 1 nil))))
                                         (cons (+ miller 1) (append (cdr m) (cdr mresults)))))
                                     (cons 1 nil))))
                              (cons (+ fletcher 1) (append (cdr f) (cdr fresults)))))
                          (cons 1 nil))))
                   (cons (+ cooper 1) (append (cdr c) (cdr cresults)))))
               (cons 1 nil))))
        (cons (+ baker 1) (append (cdr b) (cdr bresults)))))
    (cons 1 nil))))