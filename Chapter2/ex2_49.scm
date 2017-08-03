#lang sicp
(#%require sicp-pict)

; Using inbuilt functions

; Ans a
(define (outline)
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0.99 0))
         (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
         (make-segment (make-vect 0.99 0.99) (make-vect 0 0.99))
         (make-segment (make-vect 0 0.99) (make-vect 0 0)))))

; Ans b
(define (cross)
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0.99 0.99))
         (make-segment (make-vect 0.99 0) (make-vect 0 0.99)))))

; Ans c
(define (diamond)
  (segments->painter
   (list (make-segment (make-vect 0 0.5) (make-vect 0.5 0.99))
         (make-segment (make-vect 0.5 0.99) (make-vect 0.99 0.5))
         (make-segment (make-vect 0.99 0.5) (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

; Ans d (copied ;))
(define (wave)
   (segments->painter (list 
                       (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                       (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                       (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                       (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                       (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                       (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                       (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                       (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                       (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                       (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                       (make-segment (make-vect .4 1) (make-vect .6 1)) 
                       (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                       (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                       (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                       (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                       (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                       (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                       (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                       (make-segment (make-vect .75 0) (make-vect .6 0)) 
                       (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                       (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                       (make-segment (make-vect .4 0) (make-vect .25 0)) 
                       )))