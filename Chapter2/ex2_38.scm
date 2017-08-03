; (fold-right / 1 (list 1 2 3)) => 1.5
; (fold-left  / 1 (list 1 2 3)) => 0.16666666666666666
; (fold-right list nil (list 1 2 3)) => (1 (2 (3 ()))) 
; (fold-left  list nil (list 1 2 3)) => (((() 1) 2) 3) 
;
; If op is commutative, then fold-right == fold-left.
