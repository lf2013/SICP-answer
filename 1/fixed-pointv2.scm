(define (s f x);非阻尼算法
	(newline)
	(display x)
	(let ((xv (f x)))
		(if (< (abs (- x xv)) 0.001) 
			x
		(s f xv)
		)
	)
)	
(define (deal x)
	(/ (log 1000) (log x)))
(define (deal2 x)
	(/ (+ x (/ (log 1000) (log x))) 2.0)
)
	
(define (sqr y)
	(s (lambda (x) (/ (+ x (/ y x)) 2.0)) 1.0)
)
