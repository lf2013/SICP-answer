; 2.14
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (cdr x) (car x)))
(define (lower-bound x) (min (car x) (cdr x)))
(define (print-interval x)
    (display "(")
    (display (lower-bound x))
    (display ", ")
    (display (upper-bound x))
    (display ")")
    (newline))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1  p2 p3 p4))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

(define (div-internal x y)
    (if (> 0 (* (upper-bound y) (lower-bound y)))
        (error "bad bound")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

; above are picked form 2.11

(define (percent x)
    (/
        (/ (- (upper-bound x) (lower-bound x)) 2)
        (center x)))

(define (center x)
    (/ (+ (upper-bound x) (lower-bound x)) 2))

; above are picked form 2.12

(define (par1 r1 r2)
    (div-internal (mul-interval r1 r2)
        (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
    (div-internal one
        (add-interval
            (div-internal one r1)
            (div-internal one r2)))))

(define (try)
    (define r1 (make-interval 99 101))
    (define r2 (make-interval 99 101))
    (define p1 (par1 r1 r2))
    (define p2 (par2 r1 r2))
    (display "percent r1 ")
    (display (percent r1))
    (newline)
    (display "percent r2 ")
    (display (percent r2))
    (newline)
    (print-interval p1)
    (display "percent p1 ")
    (display (percent p1))
    (newline)
    (print-interval p2)
    (display "percent p2 ")
    (display (percent p2))
    (newline)
    (define p1p1 (div-internal p1 p1))
    (print-interval p1p1)
    (display "percent p1p1 ")
    (display (percent p1p1))
    (newline)
    (define p1p2 (div-internal p1 p2))
    (print-interval p1p2)
    (display "percent p1p2 ")
    (display (percent p1p2))
    (newline)

    (print-interval r1)
    (newline)
    (print-interval (div-internal (make-interval 1 1) r1))
    (newline)
    (print-interval (div-internal (make-interval 1 1) (div-internal (make-interval 1 1) r1)))
)
; conlusion
; (percent p1) = (percent p2) + (percent r1) + (percent r2)
; par2 is the right solution;
;  par2 =  a / b
;  par1 = (r1 * r2 * a) / (r1 * r2 * b)
; r1 * r2 brings the (percent r1) + (percent r2) mistake.
