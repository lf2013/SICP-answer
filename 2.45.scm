; the things worth doing typically take time and effort.
; 2.45.scm

(define (up-split painter n)
    (if (= n 0)
        painter
    (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
    (if (= n 0)
        painter
    (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (split op1 op2)
    (lambda (painter n)
        (if (= n 0)
            painter
        (let ((smaller (split painter (- n 1))))
            (op1 painter (op2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
