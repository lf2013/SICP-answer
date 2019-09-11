 ; thingsworth doing typically take time and effort.
; 4.9

(define (for? exp) (tagged-list? exp 'for))
(define (for-start clause) (car clause))
(define (for-end clause) (cadr clause))
(define (for-body clause) (caddr clause))
(define (make-for start condx update for-body)
	(if (true? (condx start))
		((for-body start) (make-for (update start) condx update for-body)))
)
; for (i = 1; i < 5; i++) {
; 	i;
; }
(make-for 1 (lambda (x) (< x 5)) (lambda (x) (+ 1 x)) (lambda (x) (x)))
