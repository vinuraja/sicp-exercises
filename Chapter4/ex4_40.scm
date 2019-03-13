#lang sicp

(#%require racket/include)
(include "amb.scm")

; Without distinct we have 5^5 = 3120 sets, and with distinct,
; we have 5! = 120 sets. A very large reduction!

; Changing the let to let*, so that each var can depend on the
; one before it. And then setting up the considered sets by pruning
; certain numbers from the start, reduces the require-count to 80!
(define (multiple-dwelling)
  (let* ((baker (amb 1 2 3 4))
         (cooper
          (amb
           (an-integer-between 2 (- baker 1))
           (an-integer-between (+ baker 1) 4)))
         (fletcher
          (amb
           (an-integer-between 2 (- cooper 2))
           (an-integer-between (+ cooper 2) 4)))
         (miller (an-integer-between (+ cooper 1) 5))
         (smith (amb
                 (an-integer-between 1 (- fletcher 2))
                 (an-integer-between (+ fletcher 2) 5))))
    (require
      (distinct? (list baker cooper fletcher 
                       miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

; The authors might have wanted us to write the requirements after each
; variable selection instead of selecting all variables, and then doing
; restrictions. I'm unsure, so posting this solution here from
; http://community.schemewiki.org/?sicp-ex-4.40
(define (nested-multiple-dwelling) 
   (let ((fletcher (amb 1 2 3 4 5))) 
     (require (not (= fletcher 5))) 
     (require (not (= fletcher 1))) 
     (let ((cooper (amb 1 2 3 4 5)) 
           (baker (amb 1 2 3 4 5))) 
       (require (not (= baker 5))) 
       (require (not (= cooper 1))) 
       (let ((miller (amb 1 2 3 4 5))) 
         (require (> miller cooper)) 
         (let ((smith (amb 1 2 3 4 5)))
           (require (not (= (abs (- fletcher cooper)) 1))) 
           (require (not (= (abs (- smith fletcher)) 1))) 
           (require 
            (distinct? (list baker cooper fletcher miller smith))) 
           (list (list 'baker baker) 
                 (list 'cooper cooper) 
                 (list 'fletcher fletcher) 
                 (list 'miller miller) 
                 (list 'smith smith)))))))