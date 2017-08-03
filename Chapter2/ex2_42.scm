(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define nil (list))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1)
             high))))

; Representation of a position of a queen would be (r, c) where r is row
; and c is column and 1 <= r < n, and 0 <= c < n. And rest-of-queens would be
; a list of these.

(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (conflicting? pos rest)
  (and (not (null? rest))
       (let ((other (car rest)))
           (or (= (car pos) (car other))
               (= (cadr pos) (cadr other))
               (= (- (car pos) (car other)) (- (cadr pos) (cadr other)))
               (= (- (car other) (car pos)) (- (cadr pos) (cadr other)))
               (conflicting? pos (cdr rest))))))

(define (safe? k positions)
  ;(display positions)
  ;(newline)
  (not (conflicting? (car positions) (cdr positions))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; (accumulate (lambda (x y) (+ y 1)) 0 (queens 8)) => 92

