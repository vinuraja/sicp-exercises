#lang sicp

; Similar to fold-left and fold-right.

(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
             (cons left (list-of-values-l2r
                          (rest-operands exps) 
                          env)))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-r2l 
                     (rest-operands exps) 
                     env)))
        (cons (eval (first-operand exps) env) right))))