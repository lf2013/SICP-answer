; 2.37
;(define (map proc l)
;    (if (null? l)
;        (list)
;    (cons (proc (car l))
;        (map proc (cdr l)))))

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

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (accumulate cons (list) (map (lambda (x) (dot-product x v)) m))
)

(define (transpose m)
    (accumulate-n cons (list) m))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define (try)
    (define va (list 1 2 1))
    (define vb (list 2 2 1))
    (define ma (list (list 1 2 3) (list 1 1 2) (list 1 1 1)))
    (display (dot-product va vb))
    (newline)
    (display (matrix-*-vector ma va))
    (newline)
    (display (transpose ma))
    (newline)
    (display (matrix-*-matrix ma ma))
)
