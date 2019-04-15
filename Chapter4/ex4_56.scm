#lang sicp
;1. (and (supervisor ?name (Bitdiddle Ben))
;        (address ?name ?addr))
;
;2. (and (salary (Bitdiddle Ben) ?bensal)
;        (salary ?pname ?psal)
;        (lisp-value < ?psal ?bensal))
;
;3. (and (supervisor ?pname ?supname)
;        (not (job ?supname '(computer . ?type)))
;        (job ?supname ?jobname))