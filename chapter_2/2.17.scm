; 2.17
(define (last-pair l)
    (if (null? (cdr l))
        l
     (last-pair (cdr l))))

(define (try)
    (last-pair (list 23 52 143 34)))
