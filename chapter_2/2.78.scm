; things worth doing typically take time and effort
; 2.78

(define (attach-tag type-tag contents)
    (if (number? contents) (contents)
        (cons type-tag contents)))

(define (type-tag datum)
    (if (number? datum)
        "scheme-number"
        (if (pair? datum) (car datum)(error "bad tag"))))

(define (contents datum)
    (if (number? datum) datum
        (if (pair? datum) (cdr datum) (error "bad tag"))))
