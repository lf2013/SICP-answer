; the things worth doing typically take time and effort.
; 2.48.scm

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect a b)
        (make-vect
            (+ (xcor-vect a) (xcor-vect b))
            (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
        (make-vect
            (- (xcor-vect a) (xcor-vect b))
            (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s v)
        (make-vect
            (* (xcor-vect v) s)
            (* (ycor-vect v) s)))

(define (make-segment vstart vend) (cons vstart vend))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
