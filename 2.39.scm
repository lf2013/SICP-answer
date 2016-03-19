; 2.39
(define (append list1 list2)
    (if (null? list1)
        list2
    (cons (car list1) (append (cdr list1) list2))))

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

(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) (list) sequence))

(define (reverse-v2 sequence)
    (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (try)
    (display (reverse (list 1 2 3 4)))
    (newline)
    (display (reverse-v2 (list 1 2 3 4)))
)
