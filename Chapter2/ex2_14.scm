; Demonstration:
; (define x (make-center-percent 10 0.1))
; (define y (make-center-percent 20 0.2))
; (par1 x y) => (4.114285714285714 . 10.56)
; (par2 x y) => (5.76 . 7.542857142857144)
;
; Assuming the interval is represented as (c, p)
; a center c and percent width p of c (where p is
; relatively small), every division and multiplication
; operation on 2 such intervals, causes the percent
; widths to be added, whereas it remains between the
; percents of the 2 intervals in the case of addition
; and subtraction.
;
; So the aim should be to reduce the number of
; arithmetic operations involved in the equation,
; so as to reduce the error width. 
;
