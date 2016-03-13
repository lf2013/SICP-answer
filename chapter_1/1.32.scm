; 1.32
; recursive version
(define (mul a b)
    (* a b))

(define (product term a next b)
    (accumulate mul 1 term a next b))

; iter version
(define (product-iter term a next b)
    (accumulate-iter mul 1 term a next b))

(define (inc a)
    (+ a 1))

(define (f a)
    (if (even? a)
        (/ (+ a 2) (+ a 1))
        (/ (+ a 1) (+ a 2))))

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
            (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
(iter a null-value))
