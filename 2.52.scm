; the things worth doing typically take time and effort.
; 2.51

(define (corner-split painter n)
    (if (= n 0)
        painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
        (let ((top-left up) ; update here
              (bottom-right right)
            (corner (corner-split painter (- n 1))))
            (beside (below painter top-left)
                    (below bottom-right corner))))))

(define (square-limit painter n)
    (let ((quarter (corner-split painter n)))
            (let ((half (beside (filp-horiz quarter) quarter)))
                 (below (flip-vert half) half))))


(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let ((top (beside (tl painter) (tr painter)))
             (bottom (beside (bl painter) (br painter))))
            (below bottom top))))

(define (square-limit painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                   rotate180  filp-vert)))
        (combine4 (corner-split painter n))))

(define (square-limit-v2 painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                    filp-vert rotate180)))
        (combine4 (corner-split painter n))))
