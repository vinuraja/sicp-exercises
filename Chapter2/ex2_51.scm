#lang sicp
(#%require sicp-pict)

(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (vector-sub (m corner1) 
                                         new-origin)
                             (vector-sub (m corner2)
                                         new-origin)))))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-top  (transform-painter 
                       painter2
                       split-point
                       (make-vect 1 0.5)
                       (make-vect 0 1)))
          (paint-bottom (transform-painter
                         painter1
                         (make-vect 0 0)
                         (make-vect 1 0)
                         split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below-using-beside painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
