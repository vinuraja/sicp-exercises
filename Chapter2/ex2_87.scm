(put '=zero? '(polynomial)
     (lambda (p)
       ((or (empty-termlist? (term-list p))
            (and (= (order (first-term (term-list p))) 0)
                 (=zero? (coeff (first-term (term-list p)))))))))

