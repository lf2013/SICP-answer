; 2.33
(define (accumulate p init sequence)
    (if (null? sequence)
        init
    (p (car sequence)
       (accumulate p init (cdr sequence)))))

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (try)
    (display (accumulate + 0 (list 1 2 3 )))
    (newline)
    (display (map (lambda (x) (* x x)) (list 1 2 3)))
    (newline)
    (display (append (list 1 2 3) (list 4 5 6)))
    (newline)
    (display (length (list 1 2 3)))

)
