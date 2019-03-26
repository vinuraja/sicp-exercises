#lang sicp

; Let's take an example parsing method:
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; If the amb evaluator evaluates from right to left,
; then we will have an infinite recursion of maybe-extend
; method calls.

; Also if you take:
(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

; If parse-verb-phrase is evaluated first, then it will
; break parsing because the input, our English sentence
; has verbs coming after the nouns. English grammar requires
; us to honor an order of parsing.