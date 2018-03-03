; things worth doing typically take time and effort

; import from book

(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)
            ((eq? m 'serializer) 
             balance-serializer)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer))
          (id1 ((account1 'serializer) 'id))
          (id2 ((account2 'serializer) 'id)))
        (if (< id1 id2)
            ((serializer2 (serializer1 exchange))
                account1
                account2)
            ((serializer1 (serializer2 exchange))
                account1
                account2))))

