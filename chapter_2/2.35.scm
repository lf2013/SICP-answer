; 2.35

(define (map proc l)
    (if (null? l)
        (list)
    (cons (proc (car l))
        (map proc (cdr l)))))

(define (accumulate p init sequence)
    (if (null? sequence)
        init
    (p (car sequence)
       (accumulate p init (cdr sequence)))))

(define (count-leaves tree)
    (accumulate + 0 (map
                (lambda (x) (
                    cond
                    ((null? x) 0)
                    ((not (pair? x)) 1)
                    ((count-leaves x))))
        tree)))

(define (try)
    (display (count-leaves (list 1 2 3 (list 1 2 3))))
)
