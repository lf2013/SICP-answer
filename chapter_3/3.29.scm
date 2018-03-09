; things worth doing typically take time and effort

; import from 3.28
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

; delay time = inverter-gate-delay * 2 + and-gete-delay + inverter-gate-delay
(define (or-gete a1 a2 out)
	(let ((na1 (make-wire)) (na2 (make-wire)) (all (make-wire)))
		(inverter-gate a1 na1)
		(inverter-gate a2 na2)
		(and-gate na1 na2 all)
		(inverter-gate all out)))

