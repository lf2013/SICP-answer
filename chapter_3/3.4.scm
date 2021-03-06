; things worth doing typically take time and effort
; 3.4
(define (make-account passwd balance)
    (let ((total-wrong 0))
        (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       (set! total-wrong 0) balance)
                "Insufficient funds"))

        (define (call-the-cops) "call-the-cops")

        (define (deposite amount)
            (begin (set! balance (+ balance amount))
                   (set! total-wrong 0) balance))

        (define (incorrect amount) (begin
                (set! total-wrong (+ 1 total-wrong))
                (if (>= total-wrong 7) (call-the-cops) "Incorrect password")))

        (define (dispatch input-passwd op)
            (if (eq? input-passwd passwd)
                (cond ((eq? op 'withdraw) withdraw)
                      ((eq? op 'deposite) deposite)
                      (else (error "Unknown request")))
            incorrect))

    dispatch))

(define (try)
    (define acc (make-account 'passwd 100))
    (display ((acc 'passwd 'withdraw) 50))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
    (newline)
    (display ((acc 'passwd 'withdraw) 50))
    (newline)
    (display ((acc 'worng 'withdraw) 60))
)

(try)
