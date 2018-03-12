; things worth doing typically take time and effort.
; 4.1

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let* ((left (eval (first-operand exps) env))
            (right (list-of-values 
             (rest-operands exps) 
             env)))
		(cons left right))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let* ((right (list-of-values 
             (rest-operands exps) 
             env))
			(left (eval (first-operand exps) env)))
		(cons left right))))
