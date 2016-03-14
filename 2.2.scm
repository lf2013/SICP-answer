; 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (midpoint p1 p2)
    (make-point (/ (+ (x-point p1) (x-point p2)) 2.0)
                (/ (+ (y-point p1) (y-point p2)) 2.0)))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")"))

(define (make-segment ps pe) (cons ps pe))
(define (start-point s) (car s))
(define (end-point s) (cdr s))
(define (midpoint-segment s)
    (midpoint (start-point s) (end-point s)))

