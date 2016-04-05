; 2.5
(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car x)
    (if (= (remainder x 2) 0)
        (+ 1 (car (/ x 2)))
        0))

(define (cdr x)
    (if (= (remainder x 3) 0)
        (+ 1 (cdr (/ x 3)))
        0))

(define (try)
    (define x (cons 3 5))
    (display (car x))
    (display " ")
    (display (cdr x))
)
