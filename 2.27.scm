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
              (else (revsese-iter (cdr l) (cons (revsese-iter (car l) (list)) s)))))
              ;(else (revsese-iter (cdr l) (cons (revsese-deep (car l)) s)))))
    (revsese-iter l (list)))

(define (append list1 list2)
    (if (null? list1)
        list2
    (cons (car list1) (append (cdr list1) list2))))

(define (revsese-deep-v2 l)
    (cond ((null? l) l)
          ((not (pair? l)) l)
          (else (revsese (cons (revsese-deep-v2 (car l)) (revsese (revsese-deep-v2 (cdr l))))))))

(define (map proc l)
    (if (null? l)
        (list)
    (cons (proc (car l))
        (map proc (cdr l)))))

(define (revsese-deep-v3 l)
    (if (pair? l)
        (revsese (map revsese-deep-v3 l))
        l))

(define x (list (list 1 2) (list 3 4 5) (list (list 6 7) (list 8 9))))
(define (try)
    (display (revsese x))
    (newline)
    (display (revsese-deep x))
    (newline)
    (display (revsese-deep-v2 x))
    (newline)
    (display (revsese-deep-v3 x))
)
