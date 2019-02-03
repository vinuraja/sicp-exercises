;#lang sicp
(#%require racket/include)
(include "table.scm")
(#%require (only racket/base time))

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

(define (vanilla-eval exp env)
  ; For debugging, uncomment below lines.
  ;(display exp)
  ;(display "=>")
  ;(newline)
  (let ((evaled-exp (cond ((self-evaluating? exp) 
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
                 type: EVAL" exp)))))
    ; For debugging, uncomment below lines.
    ;(display evaled-exp)
    ;(newline)
    evaled-exp))

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

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)    ; formal parameters
       (cddr exp)))) ; body

(define (install-assignment-package)
  (define (eval-assignment exp env)
    (set-variable-value! 
     (assignment-variable exp)
     (eval (assignment-value exp) env)
     env)
    'ok)
  (put 'eval 'set! eval-assignment))
(install-assignment-package)

(define (install-definition-package)
  (define (eval-definition exp env)
    (define-variable! 
      (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)
  (put 'eval 'define eval-definition))
(install-definition-package)

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

(define (install-letrec-package)
  (define (eval-letrec exp env)
    (eval (letrec->let exp) env))
  (put 'eval 'letrec eval-letrec))
(install-letrec-package)

(define (make-let definitions body)
  (cons 'let (cons definitions body)))

(define (let*-var-definitions exp)
  (cadr exp))
(define (let*-body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (define (make-nested-lets definitions)
    (if (null? (cdr definitions))
        (make-let (list (car definitions))
                  (let*-body exp))
        (make-let (list (car definitions))
                  (list (make-nested-lets (cdr definitions))))))
  (make-nested-lets (let*-var-definitions exp)))

(define (let? exp)
  (tagged-list? exp 'let))

(define (make-proc-call proc args)
  (append (list proc) args))

(define (let-var-definitions exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-vars exp)
  (map (lambda (x) (car x)) (let-var-definitions exp)))

(define (let-exps exp)
  (map (lambda (x) (cadr x)) (let-var-definitions exp)))

(define (letrec-body exp)
  (let-body exp))

(define (letrec-vars exp)
  (let-vars exp))

(define (letrec-exps exp)
  (let-exps exp))

(define (named-let? exp)
  (not (pair? (cadr exp))))

(define (named-let-var exp)
  (cadr exp))

(define (named-let-vars exp)
  (let-vars (cdr exp)))

(define (named-let-exps exp)
  (let-exps (cdr exp)))

(define (named-let-body exp)
  (cdddr exp))

(define (make-definition var value)
  (list 'define var value))

(define (let->combination exp)
  (if (named-let? exp)
      ;      (make-begin (list (make-definition (named-let-var exp)
      ;                                         (make-lambda (named-let-vars exp)
      ;                                                      (named-let-body exp)))
      ;                        (make-proc-call (named-let-var exp)
      ;                                        (named-let-exps exp))))
      (make-let (list (list 'rec-fn
                            (make-lambda (append '(rec-fn) (named-let-vars exp))
                                         (list (make-let (list (append (list (named-let-var exp))
                                                                 (list (make-lambda (named-let-vars exp)
                                                                                    (list (append '(rec-fn rec-fn)
                                                                                            (named-let-vars exp)))))))
                                                   (named-let-body exp))))))
                (list (append '(rec-fn rec-fn) (named-let-exps exp))))
      (make-proc-call (make-lambda (let-vars exp) (let-body exp))
                      (let-exps exp))))

(define (letrec->let exp)
  (make-let (map (lambda (var)
                   (list var ''*unassigned)) (letrec-vars exp))
            (append (map (lambda (var val)
                           (list 'set! var val)) (letrec-vars exp) (letrec-exps exp))
                    (letrec-body exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (definition? exp)
  (tagged-list? exp 'define))

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
  (cons 'lambda (cons parameters body)))

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

(define (filter pred? lst)
  (cond ((null? lst) '())
        ((pred? (car lst)) (cons (car lst) (filter pred? (cdr lst))))
        (else (filter pred? (cdr lst)))))

(define (scan-out-defines body)
  (let ((vars (map definition-variable (filter definition? body)))
        (vals (map definition-value (filter definition? body)))
        (rest-of-body (filter (lambda (exp)
                                (not (definition? exp))) body)))
    (if (null? vars)
        body
        ; The incoming body is a list, so we expect to return
        ; a list as well.
        (list (make-let (map (lambda (var)
                         (list var ''*unassigned*)) vars)
                  (append
                   (map (lambda (var val)
                          (list 'set! var val)) vars vals)
                   rest-of-body))))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (make-procedure-analyze parameters body env)
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
  (let ((val (env-loop env)))
    (if (eq? val '*unassigned*)
        (error "Unassigned variable" var)
        val)))
      

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
        (list '* *)
        (list '= =)
        (list '- -)))
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
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment 
  (setup-environment))
(define genv the-global-environment)

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output 
           (eval input 
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

; Analyze based eval functions

(define (analyze-eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) 
         (analyze-quoted exp))
        ((variable? exp) 
         (analyze-variable exp))
        ((assignment? exp) 
         (analyze-assignment exp))
        ((definition? exp) 
         (analyze-definition exp))
        ((if? exp) 
         (analyze-if exp))
        ((lambda? exp) 
         (analyze-lambda exp))
        ((begin? exp) 
         (analyze-sequence 
          (begin-actions exp)))
        ((cond? exp) 
         (analyze (cond->if exp)))
        ((let? exp)
         (analyze (let->combination exp)))
        ((application? exp) 
         (analyze-application exp))
        (else
         (error "Unknown expression 
                 type: ANALYZE" 
                exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) 
    (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env)
      (set-variable-value! 
       var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze 
                (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence 
                (lambda-body exp))))
    (lambda (env) 
      (make-procedure-analyze vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application 
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment 
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else (error "Unknown procedure type: 
                      EXECUTE-APPLICATION"
                     proc))))