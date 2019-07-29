#lang sicp

(#%require racket/include)
(include "query_system.scm")

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((evaled-stream (qeval (car contents) (singleton-stream frame))))
       (if (and (not (stream-null? evaled-stream))
                (stream-null? (stream-cdr evaled-stream)))
           evaled-stream
           the-empty-stream)))
   frame-stream))

(initialize-data-base microshaft-data-base)

(put 'unique 'qeval uniquely-asserted)

(add-rule-exp!
 '(rule (has-single-report ?s)
        (and (supervisor ?p ?s)
             (unique (supervisor ?onlyp ?s)))))

(qeval-exp '(unique (job ?x (computer wizard))))
(qeval-exp '(has-single-report ?x))