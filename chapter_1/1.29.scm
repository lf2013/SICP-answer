; picked from book
(define (sum f a next b)
	(if (> a b)
		0
	(+ (f a)
		(sum f (next a) next b)
	)))

(define (i x) x)

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (scab a b)
	(sum cube a inc b))

(define (pisum a b);caculate PI
	(define (piterm x)
		(/ 1.0 (* x (+ x 2))))
	(define (pinext x)
		(+ x 4))
	(sum piterm a pinext b)
)
(define (intergral f a b dx)
	(define (nexta a)
		(+ a dx))
	(* dx (sum f (+ a (/ dx 2.0)) nexta b))
	)

;solution 1.29
(define (il f a b n)

	(define (h)
		(/ (- b a) n))

	(define (next1a a)
		(+ a (h)))

	(define (next2a a)
		(+ a (* 2 (h))))

    ;(display (h))
    ;(display (next1a a))
    ;(display (+ a (next1a a)))
    ;(display (- b (next1a a)))
	(/ (* (h)
		 (+ (sum f a next1a b)
			;(* 2 (sum f (+ a (next1a a)) next2a (- b (next1a a))))
			;(sum f (+ a (next1a a)) next1a (- b (next1a a)))
			(* 2 (sum f (next1a a) next2a (- b (h))))
			(sum f (next1a a) next1a (- b (h))))
		 )
	3.0)
)
