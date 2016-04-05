; the things worth doing typically take time and effort.
; 2.47.scm

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (try)
    (define f (make-frame 1 2 3))
    (display (origin-frame f))
    (newline)
    (display (edge1-frame f))
    (newline)
    (display (edge2-frame f))
)

(define (make-frame-v2 origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame-v2 frame) (car frame))
(define (edge1-frame-v2 frame) (cadr frame))
(define (edge2-frame-v2 frame) (cddr frame))

(define (try-v2)
    (define f (make-frame-v2 1 2 3))
    (display (origin-frame-v2 f))
    (newline)
    (display (edge1-frame-v2 f))
    (newline)
    (display (edge2-frame-v2 f))
)
