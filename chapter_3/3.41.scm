; things worth doing typically take time and effort

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance 
                (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) 
             (protected withdraw))
            ((eq? m 'deposit) 
             (protected deposit))
            ((eq? m 'balance)
             ((protected 
                (lambda () 
                  balance)))) ; serialized
            (else 
             (error 
              "Unknown request: 
               MAKE-ACCOUNT"
              m))))
    dispatch))

(define (try)
	(define a (make-account 100))
	(define add 0)
	; possible result:
	; 100
	; 300
	(parallel-execute 
					  (set! add (+ (a 'balance) (a 'balance)))
					  ((a 'deposit) add)))
