; the things worth doing typically take time and effort.
; 2.44.scm

(define (up-split painter n)
    (if (= n 0)
        painter
    (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
