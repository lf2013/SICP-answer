;1.39
(define (tan-cf x k)
	(display (tan x))
	(define (deal a b i k)
		(if (= i k) 
			1.0;怎么预测到的？
		(- (a i) (/ (b x) (deal a b (+ 1 i) k))))
	)
	(/ x (deal (lambda (n) (- (* 2 n) 1))  (lambda (n) (* n n)) 1 k))
)
	 
