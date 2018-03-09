; things worth doing typically take time and effort

; import form 3.21
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "empty queue")
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-item (cons item '())))
        (cond ((empty-queue? queue)
                (set-front-ptr! queue new-item)
                (set-rear-ptr! queue new-item)
                queue)
              (else (set-cdr! (rear-ptr queue) new-item)
                    (set-rear-ptr! queue new-item)
                    queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
            (error "empty queue"))
          (else
            (set-front-ptr! queue (cdr (front-ptr queue))) queue)))

; define of agenda
(define inverter-gate-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) 
  (car (segments agenda)))
(define (rest-segments agenda) 
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time 
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! 
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment 
                      time 
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment 
                time 
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue 
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! 
         agenda 
         (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: 
              FIRST-AGENDA-ITEM")
      (let ((first-seg 
             (first-segment agenda)))
        (set-current-time! 
         agenda 
         (segment-time first-seg))
        (front-queue 
         (segment-queue first-seg)))))


(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! 
   (+ delay (current-time the-agenda))
   action
   the-agenda))

; import from 3.28
(define (or-gate a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value
				(logical-or (get-signal a1)
							(get-signal a2))))
			(after-delay
			or-gate-delay
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
			and-gate-delay
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
				 ((eq? m 'set-signal!)
					set-my-signal!)
				 ((eq? m 'add-action!)
					accept-action-procedure!)
				 (else (error "Unknown operation" m))))
	dispatch))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item 
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (call-each procs)
	(if (null? procs)
		'done
		(begin ((car procs)) (call-each (cdr procs)))))

(define (get-signal wire)
	(wire 'get-signal))

(define (set-signal! wire new-value)
	((wire 'set-signal!) new-value))

(define (add-action! wire proc)
	((wire 'add-action!) proc))

(define (probe name wire)
  (add-action! 
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire)))))

(define (try)
	(define input-1 (make-wire))
	(define input-2 (make-wire))
	(define sum (make-wire))
	(define carry (make-wire))
	(define input-3 (make-wire))
	(probe 'input-1 input-1)
	(probe 'input-2 input-2)
	(probe 'input-3 input-3)
	(probe 'sum sum)
	(probe 'carry carry)
	; (and-gate input-1 input-2 input-3)
	(set-signal! input-1 1)
	(propagate)
	(set-signal! input-2 1)
	(half-adder input-1 input-2 sum carry)
	(propagate)
	; (set-signal! input-2 0)
	; (propagate)
)

(try)
