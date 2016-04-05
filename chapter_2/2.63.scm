; the thing worth doing typically take time and effort
; 2.63

(define (make-tree entry left right)
    (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list1 tree)
    (if (null? tree)
        '()
        (append (tree->list1 (left-branch tree))
                (cons (entry tree) (tree->list1 (right-branch tree))))))

(define (tree->list2 tree)
    (define (iter t result)
        (if (null? t)
            result
            (iter (left-branch t) (cons (entry t) (iter (right-branch t) result)))))
    (iter tree '()))

(define (try)
    (define tree1 (make-tree 7
                        (make-tree 3
                            (make-tree 1 '() '())
                            (make-tree 5 '() '()))
                        (make-tree 9 '()
                            (make-tree 11 '() '()))))

    (define tree2 (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 7
                            (make-tree 5 '() '())
                            (make-tree 9 '()
                                (make-tree 11 '() '())))))

    (define tree3 (make-tree 5
                        (make-tree 3
                            (make-tree 1 '() '())
                            '())
                        (make-tree 9
                            (make-tree 7 '() '())
                            (make-tree 11 '() '()))))

    (display (tree->list1 tree1))
    (newline)
    (display (tree->list2 tree1))
    (newline)

    (display (tree->list1 tree2))
    (newline)
    (display (tree->list2 tree2))
    (newline)

    (display (tree->list1 tree3))
    (newline)
    (display (tree->list2 tree3))
    (newline)
)
