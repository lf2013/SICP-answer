; 1.46
(define (iterative-improve good-enough? improve)
    (lambda (x)
        (if (good-enough? x)
            x
        ((iterative-improve good-enough? improve) (improve x)))))

(define (sqrt x)
    (define (sqrt-good-enough? guess)
        (display guess)
        (newline)
        (< (abs (- (square guess) x)) 0.001))
    (define (sqrt-improve guess)
        (/ (+ (/ x guess) guess) 2))
    ((iterative-improve sqrt-good-enough? sqrt-improve) 1.0))

(define (sqrt x)
    (define (sqrt-good-enough? guess)
        (display guess)
        (newline)
        (< (abs (- (square guess) x)) 0.001))
    (define (sqrt-improve guess)
        (/ (+ (/ x guess) guess) 2))
    ((iterative-improve sqrt-good-enough? sqrt-improve) 1.0))

;(define (fixed-point f first-guess)
;    (define (close-enough? a b)
;        (< (abs (- a b)) 0.0001))
;    (if (close-enough? (f first-guess) first-guess)
;        first-guess
;        (fixed-point f (f first-guess))))

(define (fixed-point f first-guess)

    (define (fix-good-enough? x)
        (< (abs (- x (f x))) 0.0001))

    (define (fix-improve x)
        (f x))

    ((iterative-improve fix-good-enough? fix-improve) first-guess))
