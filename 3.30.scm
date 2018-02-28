; things worth doing typically take time and effort

; import from 3.28
(define (or-gate a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value
				(logical-or (get-signal a1)
							(get-signal a2))))
			(after-delay
			or-gete-delay
			(lambda ()
				(set-signal! output new-value)))))
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok)

(define (logical-or a1 a2)
	(if (and (= a1 0) (= a2 0)) 0 1))

; import from 3.29
(define (inverter-gate a1 output)
	(define (inverter-action-procedure)
		(let ((new-value
				(logical-not (get-signal a1))))
			(after-delay
				inverter-gate-delay
				(lambda ()
					(set-signal! output new-value)))))
	(add-action! a1 inverter-action-procedure))

(define (logical-not a1)
	(if (= a1 0) 1 0))

(define (and-gate a1 a2 output)
	(define (and-action-procedure)
		(let ((new-value
				(logical-and (get-signal a1)
							(get-signal a2))))
			(after-delay
			and-gete-delay
			(lambda ()
				(set-signal! output new-value)))))
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok)

(define (logical-and a1 a2)
	(if (and (= a1 1) (= a2 1)) 1 0))

(define (half-adder a b s c)
	(let ((d (make-wire)) (e (make-wire)))
		(and-gate a b c)
		(or-gate a b d)
		(inverter-gate c e)
		(and-gate d e s)
	'ok))

(define (full-adder a b c-in sum c-out)
	(let ((c1 (make-wire)) (c2 (make-wire)) (s (make-wire)))
		(half-adder b c-in s c1)
		(half-adder a s sum c2)
		(or-gate c1 c2 c-out)
	'ok))

(define (ripple-carry-adder a-in b-in c-out)
	(define (iter-once a-in b-in c-in bit sum)
		(if (and (= a-in 0) (= b-in 0)) (+ sum (* c-in bit))
			(let ((a (make-wire)) (b (make-wire)) (ci (make-wire)) (s (make-wire)) (co (make-wire)))
			(set-signal! a (and a-in modulo 2))
			(set-signal! b (and b-in modulo 2))
			(set-signal! c c-in)
			(full-adder a b ci s co)
            (iter-once (/ a-in 2) (/ b-in 2) (get-signal ci) (* bit 2) (+ sum (* bit (get-signal s)))))))
    (iter-once int-a int-b 0 1 c-out))
