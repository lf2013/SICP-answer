; 2.36

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

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        (list)
    (cons (accumulate op init (map car seqs))
        (accumulate-n op init (map cdr seqs)))))

(define (try)
    (display (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))))
)
