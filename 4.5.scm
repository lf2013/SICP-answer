; things worth doing typically take time and effort.
; 4.5

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-new-clause? clause)
  (eq? (cond-predicate clause) 'new))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (if (cond-new-clause? first)
				(eval-if2 
					(make-if (cond-predicate first)
                	     (sequence->exp 
                	      (cond-actions first))
                	     (expand-clauses 
                	      rest)))
				(make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest)))))))

(define (eval-if2 exp env)
	(let ((if-ret (eval (if-predicate exp) env)))
  		(if (true? if-ret)
  		    (eval ((if-consequent exp) if-ret)  env)
  		    (eval (if-alternative exp) env))))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
