; 2.34
(define (accumulate p init sequence)
    (if (null? sequence)
        init
    (p (car sequence)
       (accumulate p init (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                    (+ this-coeff (* x higher-terms))
                )
                0
                coefficient-sequence))

(define (try)
    (display (horner-eval 2 (list 1 1)))
    (newline)
    (display (horner-eval 2 (list 1 3 0 5 0 1)))
    (newline)
)
