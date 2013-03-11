(define (inc x) (+ 1 x));1.41
(define (double f)
		(lambda (x) (f (f x))))
(define (try)
	(((double (double double)) inc) 5))
(define (try1)
	(( (double double) inc) 5))
(define (try2)
	(((double (double (double double))) inc) 5))
