(define (compose f g)
	(lambda (x) ((f) ((g) x))))
(define (square)
	(lambda (x) (* x x)))
(define (inc)
	(lambda (x) (+ x 1)))
(define (try)
	((compose square inc) 6))

(define (repeated f n)
	(cond ((= n 1) f)
		  (else (compose f (repeated f (- n 1))))))
(define (square)
	(lambda (x) (* x x)))
(define (try)
	((repeated square 2) 5))
