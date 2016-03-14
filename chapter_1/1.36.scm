; 1.36
(define (fixed-point f first-guess)
    (display first-guess)   ; update here
    (newline)               ; and there
    (define (close-enough? a b)
        (< (abs (- a b)) 0.0001))
    (if (close-enough? (f first-guess) first-guess)
        first-guess
        (fixed-point f (f first-guess))))

(define (try)
    (fixed-point cos 1.0))

(define (try1)
    (fixed-point (lambda (x) (- (* 2 x) 1)) 1.0))

(define (try2)
    (fixed-point (lambda (x) (+ 1 ( / 1 x))) 1.0))

(define (solution)
    (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (solution2)
    (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0))
