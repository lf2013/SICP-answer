(define (smooth f)
	(lambda (x)
		(/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3.0)))

(define (repeated f n);version 2
	(if (= 1 n)
		f 
	(compose f (repeated f (- n 1)))))
(define (compose f g)
	(lambda (x) (f (g x))))

