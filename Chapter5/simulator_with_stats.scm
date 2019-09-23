;#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-machine register-names 
                      ops 
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) 
                 register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing? #f))
    (define (dispatch message)
      (cond ((eq? message 'set-trace)
             (lambda (enable)
               (set! tracing? enable)))
            ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if tracing?
                   (begin
                     (display (list 'name '= name
                                    'old-contents '= contents
                                    'new-contents '= value))
                     (newline)))
               (set! contents value)))
            (else
             (error "Unknown request: 
                     REGISTER"
                    message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (set-trace! register enable)
  ((register 'set-trace) enable))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth 
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes 
                     '= 
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (inst-count 0)
        (tracing? #f))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () 
                         (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () 
                         (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) 
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error 
             "Multiply defined register: " 
             name)
            (set! register-table
                  (cons 
                   (list name 
                         (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val 
               (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" 
                     name))))
      (define (set-register-trace! name enable)
        (let ((register (lookup-register name)))
          (set-trace! register enable)))
      (define (print-inst-count)
        (display (list 'inst-count
                       '=
                       inst-count)))
      (define (reset-inst-count)
        (set! inst-count 0))
      (define (cancel-all-breakpoints-impl)
        (define (iter-cancel-all-breakpoints-impl insts)
          (if (not (null? insts))
              (begin
                (cancel-instruction-breakpoint! (car insts))
                (iter-cancel-all-breakpoints-impl (cdr insts)))))
        (iter-cancel-all-breakpoints-impl the-instruction-sequence))
      (define (cancel-breakpoint-impl label n)
        (define (cancel-breakpoint-after-label insts k)
          (cond ((null? insts)
                 'breakpoint-not-disabled)
                ((= k 0) (cancel-instruction-breakpoint! (car insts)))
                (else (cancel-breakpoint-after-label (cdr insts) (- k 1)))))
        (define (find-label-and-cancel-breakpoint insts)
          (cond ((null? insts)
                 'breakpoint-not-disabled)
                ((memq label (instruction-labels (car insts)))
                 (cancel-breakpoint-after-label insts (- n 1)))
                (else (find-label-and-cancel-breakpoint (cdr insts)))))
        (find-label-and-cancel-breakpoint the-instruction-sequence))
      (define (set-breakpoint-impl label n)
        (define (set-breakpoint-after-label insts k)
          (cond ((null? insts)
                 'breakpoint-not-set)
                ((= k 0) (set-instruction-breakpoint! (car insts) label n))
                (else (set-breakpoint-after-label (cdr insts) (- k 1)))))
        (define (find-label-and-set-breakpoint insts)
          (cond ((null? insts)
                 'breakpoint-not-set)
                ((memq label (instruction-labels (car insts)))
                 (set-breakpoint-after-label insts (- n 1)))
                (else (find-label-and-set-breakpoint (cdr insts)))))
        (find-label-and-set-breakpoint the-instruction-sequence))
      (define (execute skip-brkpt)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((brkpt (instruction-breakpoint (car insts))))
                (if (and (not skip-brkpt) (breakpoint-enabled? brkpt))
                    (begin
                      (display (list 'breakpoint
                                     'label '= (breakpoint-label brkpt)
                                     'n '= (breakpoint-n brkpt)))
                      (newline))
                    (begin
                      (set! inst-count (+ inst-count 1))
                      (if tracing?
                          (begin
                            (print-instruction-labels (car insts))
                            (display (instruction-text (car insts)))
                            (newline)))
                      ((instruction-execution-proc 
                        (car insts)))
                      (execute #f)))))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! 
                pc
                the-instruction-sequence)
               (execute #f))
              ((eq? 
                message 
                'install-instruction-sequence)
               (lambda (seq) 
                 (set! 
                  the-instruction-sequence 
                  seq)))
              ((eq? message 
                    'allocate-register) 
               allocate-register)
              ((eq? message 'get-register) 
               lookup-register)
              ((eq? message 
                    'install-operations)
               (lambda (ops) 
                 (set! the-ops 
                       (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'print-stack-statistics)
               (stack 'print-statistics))
              ((eq? message 'print-inst-count)
               (print-inst-count))
              ((eq? message 'reset-inst-count)
               (reset-inst-count))
              ((eq? message 'trace-on)
               (set! tracing? #t))
              ((eq? message 'trace-off)
               (set! tracing? #f))
              ((eq? message 'operations) 
               the-ops)
              ((eq? message 'set-register-trace)
               (lambda (name enable)
                 (set-register-trace! name enable)))
              ((eq? message 'set-breakpoint)
               (lambda (label n)
                 (set-breakpoint-impl label n)))
              ((eq? message 'cancel-breakpoint)
               (lambda (label n)
                 (cancel-breakpoint-impl label n)))
              ((eq? message 'cancel-all-breakpoints)
               (cancel-all-breakpoints-impl))
              ((eq? message 'proceed)
               (execute #t))
              (else (error "Unknown request: 
                            MACHINE"
                           message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))
(define (proceed-machine machine)
  (machine 'proceed))

(define (print-inst-count machine)
  (machine 'print-inst-count))
(define (reset-inst-count machine)
  (machine 'reset-inst-count))

(define (register-trace-on machine name)
  ((machine 'set-register-trace) name #t))
(define (register-trace-off machine name)
  ((machine 'set-register-trace) name #f))

(define (trace-on machine)
  (machine 'trace-on))
(define (trace-off machine)
  (machine 'trace-off))

(define (print-stack-statistics machine)
  (machine 'print-stack-statistics))

(define (get-register-contents 
         machine register-name)
  (get-contents 
   (get-register machine register-name)))

(define (set-register-contents! 
         machine register-name value)
  (set-contents! 
   (get-register machine register-name) 
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (has-label? labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        #t
        #f)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels 
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (has-label? labels next-inst)
                   (error "Label already seen: " next-inst)
                   (begin
                     (if (not (null? insts))
                         (set-instruction-labels!
                          (car insts)
                          next-inst))
                     (receive 
                      insts
                      (cons 
                       (make-label-entry 
                        next-inst
                        insts)
                       labels))))
               (receive 
                (cons (make-instruction 
                       next-inst)
                      insts)
                labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) 
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

(define (make-instruction text)
  (list text '() '() '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst)
  (cadr inst))
(define (instruction-labels inst)
  (caddr inst))
(define (instruction-breakpoint inst)
  (cadddr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-car! (cdr inst) proc))
(define (set-instruction-labels! inst label)
  (set-car! (cddr inst) (cons label (caddr inst))))
(define (set-instruction-breakpoint! inst label n)
  (set-car! (cdddr inst) (list label n)))
(define (cancel-instruction-breakpoint! inst)
  (set-car! (cdddr inst) '()))
(define (print-instruction-labels inst)
  (for-each (lambda (label)
              (display label)
              (newline))
            (instruction-labels inst)))

(define (breakpoint-label brkpt)
  (car brkpt))
(define (breakpoint-n brkpt)
  (cadr brkpt))
(define (breakpoint-enabled? brkpt)
  (not (null? brkpt)))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" 
               label-name))))

(define (make-execution-procedure 
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign 
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test 
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch 
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction 
                      type: ASSEMBLE"
                     inst))))

(define (make-assign 
         inst machine labels operations pc)
  (let ((target 
         (get-register 
          machine 
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure
        ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define 
  (make-test 
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition 
                machine
                labels
                operations)))
          (lambda () 
            (set-contents! 
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: 
                ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define 
  (make-branch 
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label 
                labels 
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: 
                ASSEMBLE"
               inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label 
                   labels
                   (label-exp-label dest))))
             (lambda () 
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register 
                   machine
                   (register-exp-reg dest))))
             (lambda ()
               (set-contents! 
                pc
                (get-contents reg)))))
          (else (error "Bad GOTO instruction: 
                        ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register 
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name 
         stack-instruction)
  (cadr stack-instruction))

(define (make-perform 
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: 
                ASSEMBLE"
               inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label 
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: 
                      ASSEMBLE"
                     exp))))

(define (make-operand-exp exp machine)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: 
                      ASSEMBLE"
                     exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp) 
  (cadr exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim 
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (make-operand-exp e machine))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))
