; things worth doing typically take time and effort.
; 4.2

; a)
; can't fine 'define

; b)

(define (eval exp env)
    (cond   ((self-evaluating? exp) exp)
            ((call? exp) (apply (eval (operator exp) (env) 
                                (list-of-values (operands exp) env))))
            (else (error "Unknown expression"))))

(define (call? exp) 
    (if (pair? exp)
        (equal? (car exp) 'call)
        false))

(define (call-operator exp) 
    (cadr exp))

(define (call-operands exp) 
    (cddr exp))
