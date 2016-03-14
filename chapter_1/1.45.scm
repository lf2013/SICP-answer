; 1.45
(define (fixed-point f first-guess)
    (display first-guess)   ; update here
    (newline)               ; and there
    (define (close-enough? a b)
        (< (abs (- a b)) 0.0001))
    (if (close-enough? (f first-guess) first-guess)
        first-guess
        (fixed-point f (f first-guess))))

(define (repeated f count)
    (if (= count 1)
        (lambda (x) (f x))
        (lambda (x) (f ((repeated f (- count 1)) x)))))

(define (average-damp f)
    (lambda (x) (/ (+ x (f x)) 2)))

(define (sqrt x)
    (fixed-point ((repeated average-damp 1) (lambda (y) (/ x y))) 1.0))


; need to write a test function like this
(define (try n x)
    (define (log2 a) (/ (log a) (log 2)))
    (fixed-point ((repeated average-damp (log2 n)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))
