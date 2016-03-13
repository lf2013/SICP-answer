; 1.33
; recursive version
(define (filter? x)
    (even? x))

(define (self x) x)

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

(define (accumulate combiner filter? null-value term a next b)
    (if (> a b)
        null-value
        (if (filter? a)
            (combiner (term a)
                (accumulate combiner filter? null-value term (next a) next b))
            (accumulate combiner filter? null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
(iter a null-value))

(define (filterd-accumulate-iter combiner filter? null-value term a next b)
    (define (iter a result)
    (if (> a b)
        result
        (if (filter? a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
(iter a null-value))

; demo
(define (try)
    ;(filterd-accumulate-iter combiner filter? null-value term a next b)
    (filterd-accumulate-iter mul filter? 1 self 1 inc 5))

; problem a
(define (smallest-divisor n)
	(find-divisor 2 n))  

(define (find-divisor i n)
	(cond ((> (* i i) n) n)
		  ((= (remainder n i) 0) i)
		  (else (find-divisor (+ 1 i) n))))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (try1)
    (filterd-accumulate-iter + prime? 0 self 2 inc 10))

; problem b
(define (gcd a b)
    (if (= 0 (remainder a b))
        b
        (gcd b (remainder a b))))


(define (try2 n)
    (define (oddself a)
        (and (< a n) (= 1 (gcd a n))))
    (filterd-accumulate-iter * oddself 1 self 1 inc n))
