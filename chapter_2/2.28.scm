; 2.28
(define x (list (list 1 2) (list 3 4)))

;(define (fringe y)
;    ;(display y)
;   (cond ((not (null? y))
;         (if (not (pair? y)) (begin (display y) y)
;         (cons (fringe (car y)) (fringe (cdr y)))))))

(define (append list1 list2)
    (if (null? list1)
        list2
    (cons (car list1) (append (cdr list1) list2))))

(define (fringe y)
    ;(display y)
    (cond ((null? y) (list))
          ((not (pair? y)) (list y))
          (else (append (fringe (car y)) (fringe (cdr y))))))

(define (try)
    (display (fringe x))
    (display (fringe (list x x)))
)
