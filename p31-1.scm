(define (fast-expt a b)
	(cond ((= b 0) 0)
		  ((= b 1) a)
		  ((= (remainder b 2) 0)
			(double (fast-expt a (/ b 2))))
		  (else
			(+ a (double (fast-expt a (/ (- b 1) 2)))))))
(define (double x)
	(* 2 x))
