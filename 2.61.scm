; the thing worth doing typically take time and effort.
; 2.61
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          ((< x (car set)) false)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
        (cond ((null? set) set)
          ((equal? x (car set)) set)
          ((< x (car set)) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2)) ()
          (let ((x1 (car set1)) (x2 (car set2)))
          (cond
            ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((> x1 x2) (intersection-set set1 (cdr set2)))
            ((< x1 x2) (intersection-set (cdr set1) set2)))
          )
    )
)

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
          (let ((x1 (car set1)) (x2 (car set2)))
          (cond
            ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
            ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
            ((< x1 x2) (cons x1 (union-set (cdr set1) set2))))
          ))
    )
)

(define (try)
    (define set1 (list 1 2 3))
    (define set2 (list 3 6 7))
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
    (display (adjoin-set 6 set2))
    (newline)
    (display (intersection-set set1 set2))
    (newline)
    (display (union-set set1 set2))
)
