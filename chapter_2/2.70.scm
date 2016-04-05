; the thing worth doing typically take time and effort
; 2.70
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

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs) '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
                (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
    (cond ((null? leaf-set) '())
          ((null? (cdr leaf-set)) (car leaf-set))
          (else (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))
    ))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

; picked from 2.68
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
    (define sample-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
    (define sample-tree (generate-huffman-tree sample-pairs))
    (define message1 '(get a job
                       sha na na na na na na na na
                       get a job
                       sha na na na na na na na na
                       wah yip yip yip yip yip yip yip yip yip
                       sha boom))
    (display sample-pairs)
    (newline)
    (define result (encode message1 sample-tree))
    (display result)
    (newline)
    (display (length result))
)
