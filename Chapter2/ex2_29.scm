(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (and
        (= (* (total-weight (branch-structure (left-branch mobile)))
              (branch-length (left-branch mobile)))
            (* (total-weight (branch-structure (right-branch mobile)))
               (total-weight (branch-length (right-branch mobile)))))
             (balanced? (branch-structure (left-branch mobile)))
             (balanced? (branch-structure (right-branch mobile))))))

; To answer 4, there would only be minimal changes,
; mainly changing the cadr-s to car-s.

