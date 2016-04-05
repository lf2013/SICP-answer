; 2.1
(define (gcd a b)
    (if (= 0 (remainder a b))
        b
    (gcd b (remainder a b))))

(define (make-rat n d)
    (let ((fn (abs n))
          (fd (abs d))
          (g (gcd (abs n) (abs d))))
        (if (> 0 (* n d))
            (cons (- (/ fn  g)) (/ fd g))
            (cons (/ fn g) (/ fd g)))))

(define (number x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (number x))
    (display "/")
    (display (denom x)))
