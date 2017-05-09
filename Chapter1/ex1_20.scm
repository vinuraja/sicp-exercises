; 4 for applicative-order, until gcd(4,2).
;
; The process generated using the normal-order evaluation is the following.
; It performs 18 remainder operations: 14 when evaluating the condition and
; 4 in the final reduction phase.

  
;  (gcd 206 40) 
;   
;  (if (= 40 0) ...) 
;   
;  (gcd 40 (remainder 206 40)) 
;   
;  (if (= (remainder 206 40) 0) ...) 
;   
;  (if (= 6 0) ...) 
;   
;  (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) 
;   
;  (if (= (remainder 40 (remainder 206 40)) 0) ...) 
;   
;  (if (= 4 0) ...) 
;   
;  (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 
;   
;  (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ...) 
;   
;  (if (= 2 0) ...) 
;   
;  (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) 
;   
;  (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ...) 
;  
; (if (= 0 0) ...) 
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 
  

