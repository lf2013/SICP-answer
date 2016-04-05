; 2.21
(define (square-list l)
    (if (null? l)
        l
    (cons (* (car l) (car l))
        (square-list (cdr l)))))

(define (try)
    (display (square-list (list 1 2 3 4 5))))

(define (map proc l)
    (if (null? l)
        (list)
    (cons (proc (car l))
        (map proc (cdr l)))))

(define (square-list2 l)
    (define (square x)
        (* x x))
    (map square l))

(define (try2)
    (display (square-list2 (list 1 2 3 4 5))))
