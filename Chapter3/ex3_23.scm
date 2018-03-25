#lang sicp
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) 
  (set-car! deque item))
(define (set-rear-ptr! deque item) 
  (set-cdr! deque item))

(define (empty-deque? deque) 
  (or (null? (front-ptr deque)) (null? (rear-ptr deque))))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an 
              empty deque" deque)
      (car (front-ptr deque))))

(define (insert-front-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else (set-cdr! (cdr new-pair) (front-ptr deque))
                (set-car! (cdr (front-ptr deque)) new-pair)
                (set-front-ptr! deque new-pair)
                deque))))

(define (insert-rear-deque! deque item)
  (let ((new-pair (cons item (cons '()'()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else (set-cdr! (cdr (rear-ptr deque))
                          new-pair)
                (set-car! (cdr new-pair) (rear-ptr deque))
                (set-rear-ptr! deque new-pair)
                deque))))

(define (delete-front-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with 
                 an empty deque" deque))
        (else (set-front-ptr! deque (cddr (front-ptr deque)))
              (if (not (null? (front-ptr deque))) (set-car! (cdr (front-ptr deque)) '()))
              deque)))

(define (delete-rear-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with 
                 an empty deque" deque))
        (else (set-rear-ptr! deque (cadr (rear-ptr deque)))
              (if (not (null? (rear-ptr deque))) (set-cdr! (cdr (rear-ptr deque)) '()))
              deque)))

(define (print-deque deque)
  (if (empty-deque? deque)
      (display '())
      (display (front-ptr deque))))