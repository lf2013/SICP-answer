; things worth doing typically take time and effort

; mutex implemention
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

; semaphore implemention

; require atomically execute
(define (test-and-set2! cell)
  (if (> (car size) (cadr max-size))
      true
      (begin (set-car! cell (+ (car size) 1))
             false)))

; require atomically execute
(define (clear2! cell) (set-car! cell (- (car cell) 1)))

(define (make-mutex2 m)
  (let ((cell (list 0 m)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set2! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear2! cell))))
    the-mutex))

; demo code
(define (try)
	(define m (make-mutex2))
	(parallel-execute (m 'acquire)
					  (m 'acquire)))

