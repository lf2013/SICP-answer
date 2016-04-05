; 2.40
; picked from 1.22
(define (smallest-divisor n)
	(find-divisor 2 n))

(define (find-divisor i n)
	(cond ((> (* i i) n) n)
		  ((= (remainder n i) 0) i)
		  (else (find-divisor (+ 1 i) n))))

(define (prime? n)
	(= n (smallest-divisor n)))

;picked from 2.37
(define (accumulate p init sequence)
    (if (null? sequence)
        init
    (p (car sequence)
       (accumulate p init (cdr sequence)))))

(define (flatmap proc sequence)
    (accumulate append (list) (map proc sequence)))

(define (prime-sum? x) (prime? (+ (car x) (cadr x))))

(define (make-pair-sum x) (list (car x) (cadr x) (+ (car x) (cadr x))))

(define (enumerate-interval i j)
    (if (> i j)
        (list)
    (cons i (enumerate-interval (+ 1 i) j))))

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (unique-pairs n))))

(define (unique-pairs n)
    (flatmap
        (lambda (i)
            (map (lambda (j) (list j i))
                (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (try)
    (display (unique-pairs 6))
    (newline)
    (display (prime-sum-pairs 6))
)
