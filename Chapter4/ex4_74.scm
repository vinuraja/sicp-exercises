#lang sicp
; 1.

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map (lambda (s)
                (stream-car s))
              (stream-filter (lambda (s)
                               (not (eq? (car s) (the-empty-stream))))
                             stream)))

; 2. It shouldn't, because the order of outputs should be the same.