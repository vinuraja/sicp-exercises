#lang sicp

(#%require racket/include)
(include "query_system.scm")

(define (first-binding frame)
  (car frame))
(define (rest-bindings frame)
  (cdr frame))
(define (empty-frame? frame)
  (null? frame))

(define (unify-frames lframe rframe)
  (cond ((eq? rframe 'failed) the-empty-stream)
        ((empty-frame? lframe) rframe)
        (else (let ((binding (first-binding lframe)))
                (unify-frames (rest-bindings lframe)
                              (extend-if-possible (binding-variable binding)
                                                  (binding-value binding)
                                                  rframe))))))

(define (unify-frame-streams lframe-stream rframe-stream)
;  (display "lframe-stream")
;  (newline)
;  (display-stream lframe-stream)
;  (display "rframe-stream")
;  (newline)
;  (display-stream rframe-stream)
  (stream-flatmap
   (lambda (lframe)
     (stream-filter
      (lambda (stream)
        (not (stream-null? stream)))
      (stream-map
       (lambda (rframe)
         (unify-frames lframe rframe))
       rframe-stream)))
   lframe-stream))

(define (conjuncts-map f conjuncts)
  (if (empty-conjunction? conjuncts)
      '()
      (cons (f (first-conjunct conjuncts))
            (conjuncts-map f (rest-conjuncts conjuncts)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

; Note that this way of doing conjunction won't work if there are conjunctions
; which depend on the previous bindings of previous conjunctions. So for example,
; not, same, etc may not work.
(define (conjoin-with-unify conjuncts frame-stream)
  (let ((conjuncted-frame-stream-list (conjuncts-map
                                       (lambda (conjunct)
                                         (qeval conjunct frame-stream))
                                       conjuncts)))
;    (map (lambda (frame-stream)
;           (display "frame-stream")
;           (newline)
;           (display-stream frame-stream))
;         conjuncted-frame-stream-list)
    (accumulate
     (lambda (conjuncted-frame-stream acc-frame-stream)
       (let ((unified-stream
              (unify-frame-streams conjuncted-frame-stream acc-frame-stream)))
;         (display "unified-stream")
;         (newline)
;         (display-stream unified-stream)
;         (display "unified-stream-done")
         unified-stream))
     (singleton-stream '())
     conjuncted-frame-stream-list)))

(initialize-data-base microshaft-data-base)

(put 'and 'qeval conjoin-with-unify)

(add-rule-exp!
 '(rule (wheel ?person)
        (and (supervisor ?middle-manager 
                         ?person)
             (supervisor ?x ?middle-manager))))

(qeval-exp '(wheel ?p))