; 1.40
(define (fixed-point f first-guess)
    (display first-guess)   ; update here
    (newline)               ; and there
    (define (close-enough? a b)
        (< (abs (- a b)) 0.0001))
    (if (close-enough? (f first-guess) first-guess)
        first-guess
        (fixed-point f (f first-guess))))

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newtons-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newtons-transform g) guess))

(define (cubic a b c)
    (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
