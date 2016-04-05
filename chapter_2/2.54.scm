; the thing worth doing typically take time and effort
; 2.54

(define (memq item x)
    (cond ((null? x) false)
          ((equal? item (car x)) x)
          (else (memq item (cdr x)))))

(define (equal? iteml itemr)
    (cond ((null? iteml) (null? itemr))
          ((not (pair? iteml)) (if (not (pair? itemr)) (eq? iteml itemr) false))
          (else (if (pair? itemr) (and (equal? (car iteml) (car itemr)) (equal? (cdr iteml) (cdr itemr))) false))))

(define (try)
    (display (equal? '(this is a list) '(this is a list)))
    (newline)
    (display (equal? '(this is a list) '(this (is a) list)))
)
