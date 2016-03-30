; things worth doing tyically take time and effort
; 2.81
; a
; recursive for ever

; b
; no, same type never need this.

; c

; TODO
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((porc (get op type-tags)))
            (if proc
                (apply proc (map contents args)) 
                (if (= 2 (length args))
                    (let ((type1 (type-tag (car args)))
                          (type2 (type-tag (cadr args))))
                    (let ((t1->t2 (get-coercion type1 type2)))
                        (if t1->t2
                             (apply-generic op (t1->t2 t1) t2)
                             (let ((t2->t1 (get-coercion type2 type1)))
                                   (if t2->t1
                                         (apply-generic op t1 (t2->t1 t2))
                                         (error "no such operation"))))))
                              (error "no such operation"))))))
