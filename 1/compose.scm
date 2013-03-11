(define (inc x) (+ 1 x));1.42
(define (compose f g)
	(lambda (x) (f (g x))))
