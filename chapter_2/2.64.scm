; the thing worth doing typically take time and effort
; 2.64

(define (make-tree entry left right)
    (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree els n)
    (if (= n 0) (cons '() els)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree els left-size)))
                (let ((left-tree (car left-result))
                      (non-left-els (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                    (let ((current-entry (car non-left-els))
                         (right-result (partial-tree (cdr non-left-els) right-size)))
                        (let ((right-tree (car right-result))
                              (remaining-els (cdr right-result)))
                            (cons (make-tree current-entry left-tree right-tree) remaining-els))))))))

(define (try)
    (display (list->tree '(1 3 5 7 9 11)))
    ;(display (partial-tree (list 1 3 5 7 9 11) 6))
)
