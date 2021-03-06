; things worth doing typically take time and effort.
; 3.22
(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '()))

    	(define (empty-queue?) (null? front-ptr))

    	(define (front-queue)
    	    (if (empty-queue?)
    	        (error "empty queue")
    	        (car front-ptr)))

    	(define (delete-queue!)
    	    (if (empty-queue?)
    	        (error "empty queue")
    	        (set! front-ptr (cdr front-ptr))))

    	(define (insert-queue!)
    	    (lambda (x)
    	        (let ((item (cons x '())))
    	            (cond ((empty-queue?)
    	                    (set! front-ptr item) (set! rear-ptr item))
    	                  (else (set-cdr! rear-ptr item) (set! rear-ptr item))))))

    	(define (dispatch m)
    	    (cond ((eq? m 'empty-queue?) (null? front-ptr))
    	          ((eq? m 'front-queue) (front-queue))
    	          ((eq? m 'insert-queue!) (insert-queue!))
    	          ((eq? m 'delete-queue!) (delete-queue!))
    	          (else (error "unkonow operatio"))))
    	dispatch))

(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (delete-queue! queue) (queue 'delete-queue!))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))

(define (try)
    (define q1 (make-queue))
    (display (empty-queue? q1))
    (newline)
    (display (insert-queue! q1 'a))
    (newline)
    (display (front-queue q1))
    (newline)
    (display (insert-queue! q1 'b))
    (newline)
    (display (front-queue q1))
    (newline)
    (display (delete-queue! q1))
    (newline)
    (display (empty-queue? q1))
    (newline)
    (display (front-queue q1))
    (newline)
    (display (delete-queue! q1))
    (newline)
    (display (empty-queue? q1))
)
