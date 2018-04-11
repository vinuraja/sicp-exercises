#lang sicp
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map (lambda (x) (car x)) argstreams))
       (apply stream-map
              (cons proc 
                    (map (lambda (x) (cdr x))
                         argstreams))))))