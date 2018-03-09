; things worth doing typically take time and effort

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

; demo code
(define (try)
	(define m (make-mutex))
	(parallel-execute (m 'acquire)
					  (m 'acquire)))

time	cell-value	process1				process2
0		false			
1		false		(car cell)
2		false								(car cell)
3		false       (set-car! cell true)
4		true	       						(set-car! cell true)
5		true		'acquire sucess			'acquire success
