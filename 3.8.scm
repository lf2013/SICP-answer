; things worth doing typically take time and effort.
; 3.8
(define f
    (let ((x 1))
    (lambda (y) (begin (set! x (* x y)) x))))

(define (try)
    ; scheme default right to left
    ; you may open either case to do the test
    (display (+ (f 0) (f 1)))
    ;(display (+ (f 1) (f 0)))
)
