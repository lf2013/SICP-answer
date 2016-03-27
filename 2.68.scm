; the thing worth doing typically take time and effort
; 2.68
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

(define (encode message tree)
    (if (null? message) '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (symbol-contain x container)
    (cond ((null? container) false)
          ((equal? x (car container)) true)
          (else (symbol-contain x (cdr container)))))

(define (encode-symbol symbol tree)
    (if (null? tree) (error "illegal tree")
    (let ((cur-symbols (symbols tree)) (l (left-branch tree)) (r (right-branch tree)))
        (if (symbol-contain symbol cur-symbols)
            (cond ((leaf? tree) '())
                  ((symbol-contain symbol (symbols l)) (cons 0 (encode-symbol symbol l)))
                  ((symbol-contain symbol (symbols r)) (cons 1 (encode-symbol symbol r))))
            (error "sysbol not contailed on the tree")
        ))))


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
    (define sample-msg '(A D A B B C A))

    (display sample-msg)
    (newline)
    (display (decode sample-message sample-tree))
    (newline)
    (display sample-message)
    (newline)
    (display (encode sample-msg sample-tree))
)
