; 1.34
(define (f g)
    (g 2))

(define (square x)
    (* x x))

(f f)
;=> (f 2)
;=> (2 2)
;;The object 2 is not applicable
