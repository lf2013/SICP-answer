(define (sd n)
	(tr 2 n)
)
(define (tr a b)
	(if (< a b)
		a
;		(cond (= (remainder b a) 0) a)
	(tr (+ a 1) b)
	)
)
