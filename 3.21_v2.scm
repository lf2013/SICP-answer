; the things worth doing typically take time and effort

(define (make-queue) (cons '() '()))

(define (head-queue-ptr q) (car q))
(define (tail-queue-ptr q) (cdr q))
(define (set-head! q v) (set-car! q v))
(define (set-tail! q v) (set-cdr! q v))
(define (empty-q? q) (null? (head-queue-ptr q)))

(define (head-queue q)
    (if (empty-q? q) (error "empty queue")
        (car (head-queue q))))

(define (insert-queue! q ov)
    (let ((v (cons ov '())))
        (if (empty-q? q)
            (begin (set-head! q v) (set-tail! q v) q)
            (begin (set-cdr! (tail-queue-ptr q) v) (set-tail! q v) q))))

(define (display-q q)
    (define (display-iter h)
        (if (null? h) (display " ")
            (begin (display (car h)) (display-iter (cdr h)))))
    (display-iter (head-queue-ptr q)))

(define (delete-q! q) 
    (if (empty-q? q) (error "empty queue")
        (set-head! q (cdr (head-queue-ptr q)))))

(define (try)
    (define q (make-queue))
    (display-q q)
    (newline)
    (insert-queue! q 'a)
    (display-q q)
    (newline)
    (insert-queue! q 'b)
    (display-q q)
    (newline)
    (delete-q! q)
    (display-q q)
    (newline)
    (delete-q! q)
    (display-q q)
    (newline)
)
