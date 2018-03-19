#lang sicp
(define (make-monitored f)
  (let ((count 0))
       (lambda (inp)
         (cond ((eq? inp "how-many-calls?") count)
               ((eq? inp "reset-count") (set! count 0))
               (else (set! count (+ count 1)) (f inp))))))