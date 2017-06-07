(define (make-center-percent c p)
  (make-interval (- c (* p c)) (+ c (* p c))))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (percent i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     (* 2 (center i))))

