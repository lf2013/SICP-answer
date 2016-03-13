(define (double x)
	(lambda (y) (x (x y))))
(define (inc x)
	(+ 1 x))
(define (try)
	(((double (double double)) inc) 5))

