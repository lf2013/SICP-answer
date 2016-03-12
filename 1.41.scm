; 1.41
(define (double f)
    (lambda (x) (f (f x))))

(define (inc x)
    (+ 1 x))
; 21
(define (try)
    (((double (double double)) inc) 5))

; descriptions as below

;(double double)
(lambda (x) (double (double x)))

;(double (double double))
(lambda (x) ((double double) (double double) x))
(lambda (x) ((((double (double (double (double x))))))))
;(double (lambda (x) (double (double x))))

