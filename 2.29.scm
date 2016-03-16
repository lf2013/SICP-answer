; 2.29
(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))

(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))

(define (ismobile? m)
    (pair? (right-branch m)))

(define (total-weight m)
    (if (ismobile? m)
        (+ (total-weight (left-branch m))
           (total-weight (right-branch m)))
        (branch-structure m)))

(define (isblance? m)
    (if (ismobile? m)
        (and (isblance? (left-branch m))
             (ismobile? (right-branch m))
             (= (* (total-weight (left-branch m)) (branch-length (left-branch m)))
                (* (total-weight (right-branch m)) (branch-length (right-branch m)))))
        true))

; version 2
; 2.29
(define (make-mobile left right)
    (cons left right))

(define (make-branch length structure)
    (cons length structure))

(define (left-branch m) (car m))
(define (right-branch m) (cdr m))

(define (branch-length b) (car b))
(define (branch-structure b) (cdr b))

(define (ismobile? m)
    (pair? (right-branch m)))

(define (total-weight m)
    (if (ismobile? m)
        (+ (total-weight (left-branch m))
           (total-weight (right-branch m)))
        (branch-structure m)))

(define (isblance? m)
    (if (ismobile? m)
        (and (isblance? (left-branch m))
             (ismobile? (right-branch m))
             (= (* (total-weight (left-branch m)) (branch-length (left-branch m)))
                (* (total-weight (right-branch m)) (branch-length (right-branch m)))))
        true))
