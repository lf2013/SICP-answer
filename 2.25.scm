; 2.25
(define (try)
    ;(1 3 (5 7) 9)
    (define a (list 1 3 (list 5 7) 9))
    (display a)
    (newline)
    (display (car (cdr (car (cdr (cdr a))))))
    (newline)

    (define b (list (list 7)))
    (display b)
    (newline)
    (display (car (car b)))
    (newline)

    (define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7))))))))
    (display c)
    (newline)
    (display (car (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))))
    (newline)
)

