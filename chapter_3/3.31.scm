; things worth doing typically take time and effort

; define of wire
(define (make-wire)
	(let ((signal 0)
		 (action-procedures '()))
		(define (set-my-signal! new-value)
			(if (not (= new-value signal))
				(begin (set! signal new-value)
						(call-each action-procedures))
			'done))
		(define (accept-action-procedure! proc)
			(set! action-procedures (cons proc action-procedures))
			(proc))
		(define (dispatch m)
			(cond ((eq? m 'get-signal)
					signal)
				 ((eq? m 'set-signal)
					set-my-signal!)
				 ((eq? m 'add-action!)
					accept-action-procedure!)
				 (else (error "Unknown operation" m))))
	dispatch))

; course we should take initial state into account

(define (half-adder a b s c)
	(let ((d (make-wire)) (e (make-wire)))
		(and-gate a b c)
		(or-gate a b d)
		(inverter-gate c e)
		(and-gate d e s)
	'ok))

; if we won't call the proc immediately, the output value s, c would always be 0
