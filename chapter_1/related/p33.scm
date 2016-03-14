(define (smallest-divisor n)
	(find-divisor 2 n))  
(define (find-divisor i n)
	(cond ((> (* i i) n) n)
		  ((= (remainder n i) 0) i)
		  (else (find-divisor (+ 1 i) n))))
(define (prime? n)
	(= n (smallest-divisor n)))