; the thing worth doing typically take time and effort
; 2.74

; a
(define (get-record company user)
    ((get 'record company) user))

(define (company1-record user) (cons 1 2))
(put 'record 'company1 company1-record)

; b
(define (get-salary company user)
    ((get 'salary company) (get-record company user)))

(define (company1-salary record) (car record))
(put 'salary 'company1 company1-salary)

; c
(define (find-employee-record user company-files)
    (map (lambda (company) ((get 'record company) user)) company-files))

; d

(define (new-company-record user) (cons 1 2))
(put 'record 'new-company new-company-record)

(define (new-company-salary record) (car record))
(put 'salary 'new-company new-company-salary)
