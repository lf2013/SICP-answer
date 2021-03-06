; the thing worth doing typically take time and effort.
; 2.59
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set) set
        (cons x set)))

(define (intersection-set set1 set2)
    (if (null? set1) ()
        (if (element-of-set? (car set1) set2)
            (cons (car set1) (intersection-set (cdr set1) set2))
            (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
    (if (null? set1) set2
        (if (element-of-set? (car set1) set2)
            (union-set (cdr set1) set2)
            (cons (car set1) (union-set (cdr set1) set2)))))

(define (try)
    (define set1 (list 1 2 3))
    (define set2 (list 3 4 5))
    (display set1)
    (newline)
    (display set2)
    (newline)
    (display (element-of-set? 1 set1))
    (newline)
    (display (element-of-set? 1 set2))
    (newline)
    (display (adjoin-set 1 set2))
    (newline)
    (display (adjoin-set 5 set2))
    (newline)
    (display (intersection-set set1 set2))
    (newline)
    (display (union-set set1 set2))
)
