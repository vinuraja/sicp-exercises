#lang sicp

(#%require racket/include)
(include "query_system.scm")

(initialize-data-base microshaft-data-base)

(add-rule-exp!
 '(rule (bigshot ?p ?pdiv)
        (and (job ?p (?pdiv . ?ptype))
             (or (not (supervisor ?p ?psup))
                 (and (supervisor ?p ?sup)
                      (job ?sup (?supdiv . ?suptype))
                      (not (same ?pdiv ?supdiv)))))))

(qeval-exp '(bigshot ?p ?pdiv))