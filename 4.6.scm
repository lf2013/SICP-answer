 ; thingsworth doing typically take time and effort.
; 4.6

; (let ((<var1> <exp1>) ... (<varn> <expn>))
;   <body>)
; 
; is equivalent to
; 
; ((lambda (<var1> ... <varn>)
;    <body>)
;  <exp1>
;  
;  <expn>)

(define (let? exp) (tagged-list? exp 'let))
(define (let-exps clause) (car clause))
(define (let-body clause) (cdr clause))
(define (let->combination exp)
	(trans (let-body exp) (let-exps exp) '() '())
)

(define (trans let-body let-exps lambda-var lambda-exp)
	(cond ((null? let-exps)
		  	((make-lambda lambda-var let-body) lambda-exp))
		  (trans let-body (cdr let-exps) (cons lambda-var (caar let-exps)) (cons lambda-exp (cadr let-exps)))))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
