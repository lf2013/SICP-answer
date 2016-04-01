; things worth doing typically take time and effort
; 2.85

(define (attach-tag type-tag contents)
    (if (number? contents) (contents)
        (cons type-tag contents)))

(define (type-tag datum)
    (if (number? datum)
        'scheme-number
        (if (pair? datum) (car datum)(error "bad tag"))))

(define (contents datum)
    (if (number? datum) datum
        (if (pair? datum) (cdr datum) (error "bad tag"))))

(define (apply-generic op . args)
    (let ((types (map type-tag args)))
        (let ((proc (get op types)))
            (if proc
                (apply proc (contents args))
                (error "no such proc")
            ))))

(define (equ? l r)
    (apply-generic 'equ? l r))

(define (install-common-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add 'scheme-number (lambda (x y) (tag (+ x y))))
    (put 'equ? 'scheme-number (lambda (x y) (= x y)))
    (put 'zero 'scheme-number (lambda (x) (= x 0)))
    (put 'raise 'scheme-number (lambda (x) (make-rat x 1)))

    ; picked from 2.1
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
    (define (add-rat x y) (make-rat (+ (* (number x) (denom y)) (* (number y) (denom x)))
                                    (* (denom x) (denom y))))
    (define (equal-rat x y) (= (* (number x) (denom y)) (* (number y) (denom x))))
    (define (zero-rat x) (= 0 (number x)))
    (define (raise-rat x) (/ (* 1.0 (number x)) (denom x)))
    (define (drop-rat x) (if (= (denom x) 1) (number x) x))

    (define (tag x) (attach-tag 'rational x))
    (put 'add 'rational (lambda (x y) (tag (add-rat x y))))
    (put 'equ? 'rational (lambda (x y) (equal-rat x y)))
    (put 'zero 'rational (lambda (x) (zero-rat x)))
    (put 'raise 'rational raise-rat)
    (put 'drop 'rational drop-rat)

    (define (make-real r))
    ; TODO
)
