; 1.44
(define (smooth f)
    (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define dx 0.00001)

(define (repeated f count)
    (if (= count 1)
        (lambda (x) (f x))
        (lambda (x) (f ((repeated f (- count 1)) x)))))

(define (smooth-n-times f n)
    ((repeated smooth n) f))
