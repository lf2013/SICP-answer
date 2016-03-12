; 1.37
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

(define (d i)
    (if (= (remainder i 3) 2)
        (* 2 (/ (+ i 1) 3))
        1))

(define (try k)
    (+ 2 (cont-frac (lambda (x) 1.0)
               d
               k)))
