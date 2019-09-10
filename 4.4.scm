; things worth doing typically take time and effort.
; 4.4

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
		((or? exp)
		 (eval-or exp))
		((and? exp)
		 (eval-and exp))
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

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (eval-or exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (if (eval (first-exp exps) env) 'done 
         (eval-or (rest-exps exps) 
                        env)))))

(define (eval-and exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (if (eq? (eval (first-exp exps) env) false) 'done 
         (eval-and (rest-exps exps) 
                        env)))))
