; the thing worth doing typically take time and effort
; 2.66

(define (make-tree entry left right)
    (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (adjoin-set x set)
	(define (adjoin-set-base x set)
	  (cond ((null? set) (make-tree x '() '()))
	        ((= x (entry set)) set)
	        ((< x (entry set))
	         (make-tree 
	          (entry set)
	          (adjoin-set-base x (left-branch set))
	          (right-branch set)))
	        ((> x (entry set))
	         (make-tree
	          (entry set)
	          (left-branch set)
	          (adjoin-set-base x (right-branch set))))))
	(set! set (adjoin-set-base x set))
	; (display set)
	; (newline)
	set)

(define (lookup given-key set-of-records)
    (cond ((null? set-of-records) false)
          ((= given-key (key (entry set-of-records)))
            (entry set-of-records))
          ((> given-key (key (entry set-of-records)))
            (lookup given-key (right-branch set-of-records)))
          ((< given-key (key (entry set-of-records)))
            (lookup given-key (left-branch set-of-records)))))

(define (try)
	(define a (make-tree 1 '() '()))
	(display a)
	(newline)
	(adjoin-set 2 a)
	(adjoin-set 3 a)
	(display a)
	(newline)
)

(try)
