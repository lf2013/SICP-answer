; 2.32
(define (append list1 list2)
    (if (null? list1)
        list2
    (cons (car list1) (append (cdr list1) list2))))

(define (subsets s)
    (if (null? s) (list s)
        (let ((rest (subsets (cdr s))))
            ;(append rest (map  (lambda (x) (list (cons (car s) x))) rest)))))
            (append rest (map  (lambda (x) (cons (car s) x)) rest)))))

(define (try)
    (display (subsets (list)))
    (newline)
    (display (subsets (list 1)))
    (newline)
    (display (subsets (list 1 2 3)))
    (newline)
)
