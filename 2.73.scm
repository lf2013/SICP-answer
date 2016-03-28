; the thing worth doing typically take time and effort
; 2.73

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

; b
(define (make-sum v1 v2) (list '+ v1 v2))
(define (sum? expr) (eq? '+ (car expr)))
(define (addednd expr) (cadr expr))
(define (augend expr) (caddr expr))
(define (deriv-sum operands var)
    (make-sum
        (deriv (car operands) var) (deriv (cadr operands) var)))
(put 'deriv '+ deriv-sum)

(define (make-product v1 v2) (list '* v1 v2))
(define (product? expr) (eq? '* (car expr)))
(define (multiplier expr) (cadr expr))
(define (multiplicand expr) (caddr expr))
(define (deriv-product operands var)
    (make-sum
        (make-product (car operands) (deriv (cadr operands) var))
        (make-product (deriv (car operands) var) (cadr operands))))
(put 'deriv '* deriv-product)

; c
(define (make-exp v1 v2) (list '** v1 v2))
(define (exponentiation? expr) (eq? '** (car expr)))
(define (base expr) (cadr expr))
(define (exponent expr) (caddr expr))
(define (deriv-expr operands var)
    (let ((b (car operands)) (n (cadr operands)))
        (cond ((= n 0) (deriv 1 var))
              ((= n 1) (deriv u var))
              (else (make-product n
                      (make-product (make-exp b  (- n 1))
                          (deriv b var)))))))
(put 'deriv '** deriv-expr)

(define (deriv expr var)
    (cond ((number? expr) 0)
          ((variable? expr)
           (if (same-variable? expr var) 1 0))
          (else ((get 'deriv (operator expr)) (operands expr) var))))

(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

; d
(put '** 'deriv deriv-expr)
(put '* 'deriv deriv-product)
(put '+ 'deriv deriv-sum)

(get (operator expr) 'deriv)
