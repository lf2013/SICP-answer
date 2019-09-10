; things worth doing typically take time and effort.
; 4.3

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

; ===>
(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
		(else ((get 'eval (operator exp))
				(operands exp)
				env))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-eval-package)
	(define (quoted exp)  (text-of-quotation exp))
    (define ((assignment exp) (eval-assignment exp env)))
	(define ((if exp)  (eval-if exp env)))
	(put 'eval 'quoted quoted)
	(put 'eval 'assignment assignment)
	(put 'eval 'if if)
	; ...
    ;     ((lambda? exp)
    ;      (make-procedure 
    ;       (lambda-parameters exp)
    ;       (lambda-body exp)
    ;       env))
    ;     ((begin? exp)
    ;      (eval-sequence 
    ;       (begin-actions exp) 
    ;       env))
    ;     ((cond? exp) 
    ;      (eval (cond->if exp) env))
    ;     ((application? exp)
    ;      (apply (eval (operator exp) env)
    ;             (list-of-values 
    ;              (operands exp) 
    ;              env)))
    ;     (else
    ;      (error "Unknown expression 
    ;              type: EVAL" exp))))
'done)
