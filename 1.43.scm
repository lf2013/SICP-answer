; 1.43
; whether the count = 1 or connt > 1
; the function should return a callable object
; so when count > 1, we should return a callable object.
; the workflow was call f on the result of previous (count-1) times result.
; another idea about the problem was that:
;    parameters for f were numbers
;    parameters for repeated were functions
; such a great problem, impress me so much
(define (repeated f count)
    (if (= count 1)
        (lambda (x) (f x))
        (lambda (x) (f ((repeated f (- count 1)) x)))))

(define (square x) (* x x))

(define (inc x) (+ 1 x))

(define (try)
    ((repeated inc 10) 0))

(define (try2)
    ((repeated square 2) 5))
