; 2.38
;(define (map proc l)
;    (if (null? l)
;        (list)
;    (cons (proc (car l))
;        (map proc (cdr l)))))

(define (fold-right p init sequence)
    (if (null? sequence)
        init
    (p (car sequence)
       (fold-right p init (cdr sequence)))))

(define (fold-left p init sequence)
    (define (iter result x)
        (if (null? x)
            result
        (iter (p result (car x)) (cdr x))))
    (iter init sequence))

(define (try)
    (display (fold-right / 1 (list 1 2 3)))
    (newline)
    (display (fold-left / 1 (list 1 2 3)))
    (newline)
    (display (fold-right list (list) (list 1 2 3)))
    (newline)
    (display (fold-left list (list) (list 1 2 3)))
)
