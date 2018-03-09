; things worth doing typically take time and effort

; possible result: 100, 11, 121, 101, 110
;									   ^ !!!
(define (try)
	(define x 10)
	(parallel-execute (lambda () (set! x (* x x)))
					  (lambda () (set! x (+ 1 x))))
	(display x)
)

; with serializer
; possible result: 121, 101
(define (try2)
	(define x 10)
	(define s (make-serializer))
	(parallel-execute 
	 (s (lambda () (set! x (* x x))))
	 (s (lambda () (set! x (+ x 1)))))
)

; possible result: 121, 100, 101, 11, 110 
(define (try3)
	(define x 10)
	(define s (make-serializer))
	(parallel-execute 
	  (lambda () 
	    (set! x ((s (lambda () (* x x))))))
	  (s (lambda () (set! x (+ x 1)))))
)

(try)
