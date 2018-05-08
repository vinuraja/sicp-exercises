;#lang sicp
(#%require racket/include)
(include "table.scm")

(define (eval-old exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (exp-type exp)
  (if (pair? exp)
      (car exp)
      #f))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((get 'eval (exp-type exp))
         ((get 'eval (exp-type exp))
          exp
          env))
        ((application? exp)
         (evaluator-apply (eval (operator exp) env)
                          (list-of-values 
                           (operands exp) 
                           env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

; Can't be called apply, because we use the underlying Scheme's 'apply'
; method to run primitive procedures.
(define (evaluator-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters 
            procedure)
           arguments
           (procedure-environment 
            procedure))))
        (else
         (error "Unknown procedure 
                 type: APPLY" 
                procedure))))

(define (install-quoted-package)
  (define (eval-quoted exp env)
    (text-of-quotation exp))
  (put 'eval 'quote eval-quoted))
(install-quoted-package)

(define (install-and-package)
  (define (eval-and exp env)
    (eval-and-exps (and-exps exp) env))
  (put 'eval 'and eval-and))
(install-and-package)

(define (install-or-package)
  (define (eval-or exp env)
    (eval-or-exps (or-exps exp) env))
  (put 'eval 'or eval-or))
(install-or-package)

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (install-if-package)
  (put 'eval 'if eval-if))
(install-if-package)

(define (install-lambda-package)
  (define (eval-lambda exp env)
    (make-procedure
     (lambda-parameters exp)
     (lambda-body exp)
     env))
  (put 'eval 'lambda eval-lambda))
(install-lambda-package)

(define (install-begin-package)
  (define (eval-begin exp env)
    (eval-sequence 
     (begin-actions exp) 
     env))
  (put 'eval 'begin eval-begin))
(install-begin-package)
  
(define (install-cond-package)
  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (put 'eval 'cond eval-cond))
(install-cond-package)

(define (install-let-package)
  (define (eval-let exp env)
    (eval (let->combination exp) env))
  (put 'eval 'let eval-let))
(install-let-package)

(define (install-let*-package)
  (define (eval-let* exp env)
    (eval (let*->nested-lets exp) env))
  (put 'eval 'let* eval-let*))
(install-let*-package)

(define (make-let definitions body)
  (list 'let definitions body))

(define (let*-var-definitions exp)
  (cadr exp))
(define (let*-body exp)
  (caddr exp))

(define (let*->nested-lets exp)
  (define (make-nested-lets definitions)
    (if (null? (cdr definitions))
        (make-let (list (car definitions))
                  (let*-body exp))
        (make-let (list (car definitions))
                  (make-nested-lets (cdr definitions)))))
  (make-nested-lets (let*-var-definitions exp)))

(define (make-proc-call proc args)
  (append (list proc) args))

(define (let-var-definitions exp)
  (cadr exp))

(define (let-body exp)
  (caddr exp))

(define (let-vars exp)
  (map (lambda (x) (car x)) (let-var-definitions exp)))

(define (let-exps exp)
  (map (lambda (x) (cadr x)) (let-var-definitions exp)))

(define (let->combination exp)
  (make-proc-call (make-lambda (let-vars exp) (let-body exp))
                  (let-exps exp)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (list 'lambda parameters body))

(define (and-exps exp)
  (cdr exp))
(define (eval-and-exps exps env)
  (cond ((no-exps? exps) #t)
        ((last-exp? exps)
         (let ((eval-first-exp (eval (first-exp exps) env)))
           (if (true? eval-first-exp)
               eval-first-exp
               #f)))
        (else (if (true? (eval (first-exp exps) env))
                  (eval-and-exps (rest-exps exps) env)
                  #f))))

(define (or-exps exp)
  (cdr exp))
(define (eval-or-exps exps env)
  (cond ((no-exps? exps) #f)
        ((last-exp? exps)
         (let ((eval-first-exp (eval (first-exp exps) env)))
           (if (true? eval-first-exp)
               eval-first-exp
               #f)))
        (else
         (let ((eval-first-exp (eval (first-exp exps) env)))
           (if (true? eval-first-exp)
               eval-first-exp
               (eval-or-exps (rest-exps exps) env))))))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (no-exps? seq) (null? seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (first-action actions)
  (car actions))
(define (cond-recipient-clause? clause)
  (eq? (first-action (cond-actions clause)) '=>))
(define (cond-recipient-exp clause)
  (caddr clause))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp 
                    (cond-actions first))
                   (error "ELSE clause isn't 
                        last: COND->IF"
                          clauses)))
              ((cond-recipient-clause? first)
               (make-if (cond-predicate first)
                        (make-proc-call
                         (cond-recipient-exp first)
                         (list (cond-predicate first)))
                        (expand-clauses
                         rest)))
              (else (make-if (cond-predicate first)
                             (sequence->exp 
                              (cond-actions first))
                             (expand-clauses 
                              rest)))))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) 
  (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define apply-in-underlying-scheme apply)

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    ;(define-variable! 'true true initial-env)
    ;(define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment 
  (setup-environment))
(define genv the-global-environment)