;(define (repeated f n x);version 1
;;	(lambda (x) 
;	(if (= 1 n)
;		(f x)
;	(f (repeated f (- n 1) x))))
;;)

;(define (repeated f n);version 2
;	(if (= 1 n)
;		f 
;	(compose f (repeated f (- n 1)))))
;(define (compose f g)
;	(lambda (x) (f (g x))))

;(define (repeated f n);version 3
;	(if (= 1 n)
;		f 
;	(f (repeated f (- n 1)))))
