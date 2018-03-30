#lang sicp
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-entry key value)
  (cons key value))
(define (key entry) (car entry))
(define (val entry) (cdr entry))
(define (set-val! entry value) (set-cdr! entry value))

(define (assoc given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (assoc given-key (left-branch set-of-records)))
        (else (assoc given-key (right-branch set-of-records)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (key x) (key (entry set))) set)
        ((< (key x) (key (entry set)))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> (key x) (key (entry set)))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (lookup key table)
  (let ((record (assoc key (val table))))
    (if record
        (val record)
        #f)))

(define (insert! key value table)
  (let ((record (assoc key (val table))))
    (if record
        (set-val! record value)
        (set-val! table
                  (adjoin-set (make-entry key value) 
                              (val table)))))
  'ok)

(define (make-table)
  (make-entry '*table* '()))