; the things worth doing typically take time and effort.
; 2.49.scm

; picked from 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect a b)
        (make-vect
            (+ (xcor-vect a) (xcor-vect b))
            (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
        (make-vect
            (- (xcor-vect a) (xcor-vect b))
            (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect s v)
        (make-vect
            (* (xcor-vect v) s)
            (* (ycor-vect v) s)))

; picked from 2.47
(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

; picked from 2.48
(define (make-segment vstart vend) (cons vstart vend))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (frame-coord-map frame)
    (lambda (vect)
        (add-vect (origin-frame frame)
            (add-vect
                (scale-vect (xcor-vect vect) (edge1 frame))
                (scale-vect (ycor-vect vect) (edge2 frame))))))

(define (segment->painter segment-list)
    (lambda (frame)
        (for-each
            (lambda (segment)
                (draw-line
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))))
            segment-list)))

(define (trya)
    (define linex (make-segment (make-vect 0 0) (make-vect 0 1)))
    (define liney (make-segment (make-vect 0 0) (make-vect 1 0)))
    (define lista (list linex liney))
    (define painter-a (segment->painter lista)))

(define (tryb)
    (define linex (make-segment (make-vect 0 0) (make-vect 1 1)))
    (define liney (make-segment (make-vect 0 1) (make-vect 1 0)))
    (define lista (list linex liney))
    (define painter-a (segment->painter lista)))

(define (tryc)
    (define linex (make-segment (make-vect 0 1) (make-vect 1 0)))
    (define liney (make-segment (make-vect 1 0) (make-vect 2 1)))
    (define linez (make-segment (make-vect 0 1) (make-vect 1 2)))
    (define linew (make-segment (make-vect 1 2) (make-vect 2 1)))
    (define lista (list linex liney linez linew))
    (define painter-a (segment->painter lista)))

(define (tryd)
    false
    ; TODO
)
