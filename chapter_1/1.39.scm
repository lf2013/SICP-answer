; 1.39
(define (cont-frac n d k)
    (define (iter n d i result)
        (if (= i 0)
            result
            (iter n d (- i 1) (/ (n i) (+ (d i) result)))))
    (iter n d k 1))

(define (cont-frac2 n d k)
    (define (no-iter n d i)
        (if (= i k)
            1
            (/ (n i) (+ (d i) (no-iter n d (+ i 1))))))
    (no-iter n d 1))

(define (try k)
    (/ 1 (cont-frac (lambda (x) 1.0)
               (lambda (x) 1.0)
               k)))

(define (try2 k)
    (/ 1 (cont-frac2 (lambda (x) 1.0)
               (lambda (x) 1.0)
               k)))

(define (tan-cf x k)

    (define (f k)

        (define (m j)
            (- (* 2.0 j) 1))

        (define (iter i result)
            (display i)
            (newline)
            (if (= i 0)
                result
                (iter (- i 1) (- (m i) (/ (* x x) result)))))
    (iter k 1))
(/ x (f k)))


