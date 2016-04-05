; the thing worth doing typically take time and effort
; 2.67
(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? leaf) (equal? 'leaf (car leaf)))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (make-code-tree l r)
    (list l r (append (symbols l) (symbols r)) (+ (weight l) (weight r))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree) (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree) (weight-leaf tree)
        (cadddr tree)))

(define (decode message tree)
    (define (decode1 m t)
    (if (null? m) '()
        (let ((next-b (choose-branch (car m) t)))
            (if (leaf? next-b)
                (cons (symbol-leaf next-b) (decode1 (cdr m) tree))
            (decode1 (cdr m) next-b)))))
    (decode1 message tree))

(define (choose-branch m t)
    (cond ((= m 0) (left-branch t))
          ((= m 1) (right-branch t))
          (else error( "bad bit"))))
(define (try)
    (define sample-tree
        (make-code-tree
            (make-leaf 'A 4)
            (make-code-tree
                (make-leaf 'B 2)
                (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
    (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
    (decode sample-message sample-tree)
)


