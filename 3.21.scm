; things worth doing typically take time and effort.
; 3.21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-quque? queue) (null? (front-ptr queue)))

(define (front-queue queue)
    (if (empty-quque? queue)
        (error "empty queue")
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-item (cons item '())))
        (cond ((empty-quque? queue)
                (set-front-ptr! queue new-item)
                (set-rear-ptr! queue new-item)
                queue)
              (else (set-cdr! (rear-ptr queue) new-item)
                    (set-rear-ptr! queue new-item)
                    queue))))

(define (delete-queue! queue)
    (cond ((empty-quque? queue)
            (error "empty queue"))
          (else
            (set-front-ptr! queue (cdr (front-ptr queue))) queue)))

(define (display-queue queue)
    (define (display-iter head)
        (cond ((null? head) (display " "))
              (else (display (car head)) (display-iter (cdr head)))))
    (display-iter (front-ptr queue)))

(define (try)
    (define q1 (make-queue))
    (display-queue q1)
    (newline)
    (insert-queue! q1 'a)
    (display-queue q1)
    (newline)
    (insert-queue! q1 'b)
    (display-queue q1)
    (newline)
    (delete-queue! q1)
    (display-queue q1)
    (newline)
    (delete-queue! q1)
    (display-queue q1)
    (newline)
)

