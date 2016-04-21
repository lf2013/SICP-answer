; things worth doing typically take time and effort
; 3.14 reverse work

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
        (let ((temp (cdr x)))
            (set-cdr! x y)
            (loop temp x))))
    (loop x '()))

(define (try)
    (define a (list 'a 'b 'c 'd))
    (display a)
    (newline)
    (display (mystery a))
    (newline)
    (display a)
)
