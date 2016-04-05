; things worth doing typically take time and effort
; 3.3
(define (make-account passwd balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))

    (define (deposite amount)
        (begin (set! balance (+ balance amount)) balance))

    (define (incorrect amount) "Incorrect password")

    (define (dispatch input-passwd op)
        (if (eq? input-passwd passwd)
            (cond ((eq? op 'withdraw) withdraw)
                  ((eq? op 'deposite) deposite)
                  (else (error "Unknown request")))
        incorrect))

    dispatch)

(define (try)
    (define acc (make-account 'passwd 100))
    (display ((acc 'passwd 'withdraw) 50))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
)
