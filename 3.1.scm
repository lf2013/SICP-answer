; things worth doing typically take time and effort.
(define (make-accumulator count)
    (lambda (x) (begin (set! count (+ count x)) count)))

(define (try)
    (define A (make-accumulator 5))
    (display (A 10))
    (newline)
    (display (A 10)))
