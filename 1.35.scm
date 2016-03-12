; 1.35
(define (fixed-point f first-guess)
    (define (close-enough? a b)
        (< (abs (- a b)) 0.0001))
    (if (close-enough? (f first-guess) first-guess)
        first-guess
        (fixed-point f (f first-guess))))

(define (try)
    (fixed-point cos 1.0))

(define (try1)
    (fixed-point (lambda (x) (- (* 2 x) 1)) 1.0))

; p * p = p + 1 ==>  p = 1 + 1 / p
(define (try2)
    (fixed-point (lambda (x) (+ 1 ( / 1 x))) 1.0))

