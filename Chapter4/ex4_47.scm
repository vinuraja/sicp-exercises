#lang sicp

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 
        'verb-phrase
        (parse-verb-phrase)
        (parse-prepositional-phrase))))

; 1. I believe this would go into an infinite recursion
; of (parse-word verbs). If the first amb isn't
; chosen, we evaluate the list which evaluates (parse-verb-phrase),
; which again evaluates to (parse-word verbs), which
; will obviously fail (because the last choice of
; the same failed). So on and so forth to deeper levels.
; 2. Interchanging the order will cause a different
; infinite recursion. Invoking (parse-verb-phrase), will
; invoke the list which again invokes (parse-verb-phrase)
; and so on and so forth to deeper levels.