; the thing worth doing typically take time and effort
; 2.57

(define (deriv expr var)
    (cond ((number? expr) 0)
          ((variable? expr)
           (if (same-variable? expr var) 1 0))
          ((sum? expr)
            (make-sum
                (deriv (addend expr) var) (deriv (augend expr) var)))
          ((product? expr)
            (make-sum
                (make-product (multiplier expr) (deriv (multiplicand expr) var))
                (make-product (deriv (multiplier expr) var) (multiplicand expr))))
          ((exponentiation? expr)
            (let ((n (exponent expr)) (u (base expr)))
                (cond ((= n 0) (deriv 1 var))
                      ((= n 1) (deriv u var))
                      (else (make-product (exponent expr)
                                (make-product (make-exp (base expr) (- (exponent expr) 1))
                                    (deriv (base expr) var)))))))
          (else (error "unknown expression type -- DERIVE" expr))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum v1 v2 . w) (append (list '+ v1 v2) w))
(define (sum? expr) (eq? '+ (car expr)))
(define (addend expr) (cadr expr))
(define (augend expr)
    (let ((l (caddr expr)) (r (cdddr expr)))
        (if (null? r) l (make-sum l (car r)))))

(define (make-product v1 v2 . w) (append (list '* v1 v2) w))
(define (product? expr) (eq? '* (car expr)))
(define (multiplier expr) (cadr expr))
(define (multiplicand expr)
    (let ((l (caddr expr)) (r (cdddr expr)))
        (if (null? r) l (make-product l (car r)))))

(define (make-exp v1 v2) (list '** v1 v2))
(define (exponentiation? expr) (eq? '** (car expr)))
(define (base expr) (cadr expr))
(define (exponent expr) (caddr expr))

(define (try)
    (display (deriv '(* x y) 'x))
    (newline)
    (display (deriv '(+ x x 3) 'x))
    (newline)
    (display (deriv '(** x 3) 'x))
    (newline)
    (display (deriv '(* x y (+ x 3)) 'x))
)
