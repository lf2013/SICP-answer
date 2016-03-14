; 2.4
(define (cons x y)
    (lambda (m) (m x y)))
(define (car x)
    (x (lambda (p q) p)))
(define (cdr x)
    (x (lambda (p q) q)))

(define (try)
    (define a (cons 1 2))
    (display (car a))
    (display " ")
    (display (cdr a))
)
