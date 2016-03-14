; 2.18
(define (revsese l)
    (define (revsese-iter l s)
        (if (null? l)
            s
        (revsese-iter (cdr l) (cons (car l) s))))
    (revsese-iter l (list)))

(define (try)
    (revsese (list 1 2 3 5 2)))
