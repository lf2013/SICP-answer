; 2.27
(define (revsese l)
    (define (revsese-iter l s)
        (if (null? l)
            s
        (revsese-iter (cdr l) (cons (car l) s))))
    (revsese-iter l (list)))

(define (revsese-deep l)
    (define (revsese-iter l s)
        (cond ((null? l) s)
              ((not (pair? l)) l)
              (else (revsese-iter (cdr l) (cons (revsese-deep (car l)) s)))))
    (revsese-iter l (list)))

(define x (list (list 1 2) (list 3 4 5) (list (list 6 7) (list 8 9))))
(define (try)
    (display (revsese x))
    (display (revsese-deep x))
)
