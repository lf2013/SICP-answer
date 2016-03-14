(define (expmod base e m)
    (cond ((= e 0) 1)
          ((even? e) (remainder (square (expmod base (/ e 2) m)) m))
          (else (remainder (* base (expmod base (- e 1) m)) m))))

(define (fast-test n)
    (define (try-it a)
    (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
         ((fast-test n) (fast-prime? n (- times 1)))
         (else false)))

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (fast-prime? n 100)
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

(define (test-n n)
    (define (test-iter a b)
        (if (< a b)
            (if (= (expmod a b b) a) (test-iter (+ 1 a) b) false)
        true))
    (test-iter 1 n)
)

;(define (try)
;	(display (search-for-prime 1000 1020))
;	(display (search-for-prime 10000 10020))
;	(display (search-for-prime 100000 100020))
;	(display (search-for-prime 10000000 10000020))
;	(display (search-for-prime 100000000 100000020))
;	(display (search-for-prime 1000000000 1000000020))
;	(display (search-for-prime 10000000000 10000000020))
;	(display (search-for-prime 100000000000 100000000020))
;)
(define (try)
    (display (test-n 561))
    (display (test-n 1105))
    (display (test-n 2465))
    (display (test-n 2821))
    (display (test-n 6601))
)