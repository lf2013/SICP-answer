; 2.26
(define a (list 1 2 3))
(define b (list 4 5 6))
(define (append list1 list2)
    (if (null? list1)
        list2
    (cons (car list1) (append (cdr list1) list2))))

(define (try)
    (display (append a b))
    ;(1 2 3 4 5 6)
    (display (cons a b))
    ;((1 2 3) 4 5 6)
    (display (list a b))
    ;((1 2 3) (4 5 6))
)
