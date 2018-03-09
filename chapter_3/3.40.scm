; things worth doing typically take time and effort

; possible result:
; 10 * 10 * 10
; 10 * 10 * (10 * 10)
; 10 * (10 * 10) * (10 * 10)
; (10 * 10) * (10 * 10) * (10 * 10)

; 10 * 10
; 10 * (10 * 10 * 10)
; (10 * 10 * 10) * (10 * 10 * 10)
(define (try)
	(define x 10)
	(parallel-execute 
	 (lambda () (set! x (* x x)))
	 (lambda () (set! x (* x x x))))
)

; possible result:
; (10 * 10) * (10 * 10) * (10 * 10)
; (10 * 10 * 10) * (10 * 10 * 10)
(define x 10)
(define s (make-serializer))
(parallel-execute 
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (* x x x)))))
