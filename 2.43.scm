; 2.42
;;picked from 2.37
(define (accumulate p init sequence)
    (if (null? sequence)
        init
    (p (car sequence)
       (accumulate p init (cdr sequence)))))

(define (flatmap proc sequence)
    (accumulate append (list) (map proc sequence)))

(define (enumerate-interval i j)
    (if (> i j)
        ()
    (cons i (enumerate-interval (+ 1 i) j))))

;((1 1) (2 1)) ((1 1) (2 2))
(define (safe? positions)
    (define (safe-with-other? cur other)
        (if (null? other) true
            (let ((x1 (car cur)) (y1 (cadr cur))
                 (x2 (car other)) (y2 (cadr other)))
            (not (or (= x1 x2) (= y1 y2)
                      (= (- x1 x2) (- y1 y2))
                      (= (- x1 x2) (- y2 y1)))))))

    (define (safe-with-others cur others)
        (if (null? others) true
              (if (safe-with-other? cur (car others))
                  (safe-with-others cur (cdr others))
                    false)))

    (if (null? positions)
        true
        (safe-with-others (car positions) (cdr positions))))

(define (adjoin-position new-row k rest-of-queens)
    (append (list (list k new-row)) rest-of-queens ))

(define (queens board-size)

    (define empty-board ())

    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
                (filter
                    (lambda (positions) (safe? positions))
                    (flatmap
                        (lambda (new-row)
                            (map (lambda (rest-of-queens)
                                    (adjoin-position new-row k rest-of-queens))
                                    (queen-cols (- k 1))))
                                    ; the queen-cols was called every turn, cursing a k * T increase.
                                    ; there are n round; which result in 1 * 2 *..* n * T = n! * T
                        (enumerate-interval 1 board-size)))))
    (queen-cols board-size))

(define (try)
    (display (queens 0))
    (newline)
    (display (queens 1))
    (newline)
    (display (queens 2))
    (newline)
    (display (queens 3))
    (newline)
    (display (queens 4))
)
