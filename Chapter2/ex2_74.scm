; 1.
; You can provide type information or use the division
; name. The files can be structured any way; the 'get'
; call will return a lambda which can return the file
; based on the employee's name.
(define (get-record division-file employee)
  ((get 'get-record (division division-file) employee))

; 2.
; We use a record-type method to get the type information
; about the record and use that to dispatch to a get-salary
; method.
(define (get-salary division-file employee)
  (get 'get-salary (record-type ((get 'get-record division-file) employee))))

; 3.
; Assuming get-record returns #f on not finding the record.
(define (find-employee-record employee division-files)
  (if (null? division-files)
      #f
      (or (get-record (car division-files) employee)
          (find-employee-record employee (cdr division-files)))))

; 4.
; They would need to provide dispatch mappings for get-record
; and get-salary for the new division.
