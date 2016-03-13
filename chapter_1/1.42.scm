; 1.42
(define (compose f g)
    (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (inc x) (+ 1 x))

(define (try)
    ((compose square inc) 6))

(define (try2)
    ((compose inc square ) 6))
