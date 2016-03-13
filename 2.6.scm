; 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
    (lambda (f) (lambda (x) (f  x))))

(define two
    (lambda (f) (lambda (x) (f (f x)))))

(define (add l r)
    (lambda (f) (lambda (x) (r  f ((l f) x)))))

;(add zero zero)
;(lambda (f) (lambda (x) (zero f x)))
;(lambda (f) (lambda (x) x))

;(add one one)
(lambda (f) (lambda (x) (one f ((one f) x))))
(lambda (f) (lambda (x) (one f (f x))))
(lambda (f) (lambda (x) (f (f x))))
