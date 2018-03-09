; things worth doing typically take time and effort

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
