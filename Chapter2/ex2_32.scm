(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (cons (car s) subset)) 
                          rest)))))

; Let's take a set A of elements containing 'a'. The set
; of all subsets of A, has elements containing 'a' or not,
; so they can be formed by finding all subsets of A - {a},
; and appending 'a' to each of them and appending the whole
; thing.

