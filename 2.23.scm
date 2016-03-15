; 2.23

(define (for-each proc l)
    (if (null? l)
        (list)
    (begin (proc (car l))
     (for-each proc (cdr l)))))

(define (try2)
    (for-each (lambda (x) (newline) (display x)) (list 23 234 435 56)))
