; 2.41
;;picked from 2.37
(define (accumulate p init sequence)
    (if (null? sequence)
        init
    (p (car sequence)
       (accumulate p init (cdr sequence)))))

(define (flatmap proc sequence)
    (accumulate append (list) (map proc sequence)))

;(define (prime-sum? x) (prime? (+ (car x) (cadr x))))
;
;(define (make-pair-sum x) (list (car x) (cadr x) (+ (car x) (cadr x))))
;
(define (enumerate-interval i j)
    (if (> i j)
        ()
    (cons i (enumerate-interval (+ 1 i) j))))

;(define (unique-tuple n)
;    (flatmap
;        (lambda (i)
;            (map (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1))))
;                (enumerate-interval 1 (- i 1))))
;            (enumerate-interval 1 n)))

(define (unique-tuple-2 n)
    (accumulate append (list)
        (accumulate append (list)
            (map (lambda (i)
                (map (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1))))
                    (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (find n s)
    (define (sum-equal? x)
        (= s (+ (car x) (cadr x) (caddr x))))

    (filter sum-equal?
        (unique-tuple-2 n)))

(define (try)
    (display (unique-tuple-2 6))
    (newline)
    ;(display (prime-sum-pairs 6))
    (display (find 12 12))
)
