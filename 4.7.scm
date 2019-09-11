 ; thingsworth doing typically take time and effort.
; 4.7

; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))

; part 1
(define (let2? exp) (tagged-list? exp 'let))
(define (let2-exps clause) (car clause))
(define (let2-body clause) (cdr clause))
(define (let*->nested-lets exp)
	(trans2 (let2-body exp) (let2-exps exp)))

(define (trans2 let-body let-exps)
	(cond ((null? let-exps)
		  	let-body
		  (make-let (car let-exps) (trans2 let-body (cdr let-exps))))))


(define (let? exp) (tagged-list? exp 'let))
(define (let-exps clause) (car clause))
(define (let-body clause) (cdr clause))
(define (make-let let-exps let-body) 
		(list 'let let-exps let-body))
(define (let->combination exp)
	(trans (let-body exp) (let-exps exp) '() '())
)

(define (trans let-body let-exps lambda-var lambda-exp)
	(cond ((null? let-exps)
		  	((make-lambda lambda-var let-body) lambda-exp))
		  (trans let-body (cdr let-exps) (cons (caar let-exps) lambda-var) (cons (cadr let-exps) lambda-exp))))

; part 2
; (eval (let*->nested-lets exp) env)
