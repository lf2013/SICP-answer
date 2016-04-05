; things worth doing typically take time and effort
; 2.84

(define (apply-generic op . args)
    (define (higher? type1 type2)
        (cond ((eq? type1 type2) 0)
              ((not (get-raise type1)) 1)
              ((not (get-raise type2)) -1)
              (else (higher? (raise type1) (raise type2)))))

    (let ((type-tags (map type-tag args)))
        (let ((porc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= 2 (length args))
                    (let ((type1 (type-tag (car args)))
                          (type2 (type-tag (cadr args)))
                          (higher (higher? type1 type2)))
                    (cond ((= higher 0) (error "not defined operation for type1"))
                          ((> higher 0) (apply-generic op t1 (raise t2)))
                          ((< higher 0) (apply-generic op (raise t1) t1))))
                    (error "no such operation"))))))
