(define (smallest-divisor n)
	(find-divisor 2 n))

(define (find-divisor i n)
	(cond ((> (* i i) n) n)
		  ((= (remainder n i) 0) i)
		  (else (find-divisor (+ 1 i) n))))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " **** ")
	(display elapsed-time))

(define (even? n)
    (= (remainder n 2) 0))

(define (search-for-prime l r)
    (if (even? l)
        (search-for-prime (+ l 1) r)
    (cond ((< l r)
        (timed-prime-test l)
        (search-for-prime (+ l 2) r)))))

(define (try)
	(display (search-for-prime 1000 1020))
	(display (search-for-prime 10000 10020))
	(display (search-for-prime 100000 100020))
	(display (search-for-prime 10000000 10000020))
	(display (search-for-prime 100000000 100000020))
	(display (search-for-prime 1000000000 1000000020))
	(display (search-for-prime 10000000000 10000000020))
	(display (search-for-prime 100000000000 100000000020))
)
