; 2.12
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (cdr x) (car x)))
(define (lower-bound x) (min (car x) (cdr x)))
(define (print-interval x)
    (display "(")
    (display (lower-bound x))
    (display ", ")
    (display (upper-bound x))
    (display ")"))

(define (make-center-percent c p)
    (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define (percent x)
    (/
        (/ (- (upper-bound x) (lower-bound x)) 2)
        (center x)))

(define (center x)
    (/ (+ (upper-bound x) (lower-bound x)) 2))

(define (try)
    (define a (make-center-percent 5 0.1))
    (print-interval a)
    (newline)
    (display (percent a))
    (newline)
    (display (center a)))
