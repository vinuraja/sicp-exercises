#lang sicp

(#%require racket/include)
(include "amb.scm")

(define nouns 
  '(noun student professor cat class))

(define verbs 
  '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions 
  '(prep for to in by with))

; I had to change the implementation to be more
; functional by passing in the unparsed input,
; and returning the remaining unparsed input. With
; a mutable global *unparsed* variable, the amb
; method in sicp module wasn't working properly.

(define (parse-simple-noun-phrase unparsed)
  (let* ((parse-articles-result
          (parse-word articles unparsed))
         (parse-nouns-result
          (parse-word nouns (cdr parse-articles-result))))
    (cons (list 'simple-noun-phrase
                (car parse-articles-result)
                (car parse-nouns-result)) (cdr parse-nouns-result))))

(define (parse-prepositional-phrase unparsed)
  (let* ((parse-prep-result (parse-word prepositions unparsed))
         (parse-noun-result
          (parse-noun-phrase (cdr parse-prep-result))))
    (cons (list 'prep-phrase
                (car parse-prep-result)
                (car parse-noun-result)) (cdr parse-noun-result))))

(define (parse-noun-phrase unparsed)
  (define (maybe-extend noun-phrase)
    (amb 
     noun-phrase
     (let ((parse-prep-result
            (parse-prepositional-phrase (cdr noun-phrase))))
       (maybe-extend 
        (cons (list 'noun-phrase
                    (car noun-phrase)
                    (car parse-prep-result)) (cdr parse-prep-result))))))
  (maybe-extend (parse-simple-noun-phrase unparsed)))

(define (parse-verb-phrase unparsed)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (let ((parse-prep-result
            (parse-prepositional-phrase (cdr verb-phrase))))
       (maybe-extend
        (cons (list 'verb-phrase
                    (car verb-phrase)
                    (car parse-prep-result)) (cdr parse-prep-result))))))
  (maybe-extend (parse-word verbs unparsed)))

(define (parse-word word-list unparsed)
  (require (not (null? unparsed)))
  (require (memq (car unparsed) 
                 (cdr word-list)))
  (let ((found-word (car unparsed)))
    (cons (list (car word-list) found-word) (cdr unparsed))))

(define (parse input)
  (let ((sent (parse-sentence input)))
    (display ">>")
    (newline)
    (display (car sent))
    (newline)
    (require (null? (cdr sent)))
    (car sent)))

(define (parse-sentence unparsed-before)
  (let* ((parse-noun-result (parse-noun-phrase unparsed-before))
         (parse-verb-result (parse-verb-phrase
                             (cdr parse-noun-result)))
         (unparsed-after (cdr parse-verb-result)))
    (cons (list 'sentence
                (car parse-noun-result)
                (car parse-verb-result)) unparsed-after)))
