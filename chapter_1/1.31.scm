; 1.31
; recursive version
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

; iter version
(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

(define (inc a)
    (+ a 1))

(define (f a)
    (if (even? a)
        (/ (+ a 2) (+ a 1))
        (/ (+ a 1) (+ a 2))))
