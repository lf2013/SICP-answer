; things worth doing typically take time and effort

(define (make-table) (list 'table))

(define (look-up key table) (assoc key (cdr table)))

(define (assoc key table)
    (cond ((null? table) false)
          ((equal? key (caar table)) (car table))
          (else (assoc key (cdr table)))))

(define (insert! k v table)
    (let ((record (assoc k (cdr table))))
        (if record (set-cdr! record v)
            (set-cdr! table
                    (cons (cons k v) (cdr table))))))

(define (make-table2) (list 'table2))
(define (look-up2 key1 key2 table)
    (let ((record (assoc key1 (cdr table))))
        (if record (assoc key2 (cdr record))
            false)))

(define (insert2! key1 key2 v table)
    (let ((record (assoc key1 (cdr table))))
        (if record
            (let ((record2 (assoc key2 (cdr record))))
                (if record2 (set-cdr! record2 v)
                    (set-cdr! record (cons (cons key2 v) (cdr record)))))
            (set-cdr! table
                (cons (cons key1 (cons (cons key2 v) '())) (cdr table))))))

(define (make-table3 same-key?) (list 'table2))
(define (look-up3 key1 key2 table)
    (let ((record (assoc key1 (cdr table))))
        (if record (assoc key2 (cdr record))
            false)))

(define (insert3! key1 key2 v table)
    (let ((record (assoc key1 (cdr table))))
        (if record
            (let ((record2 (assoc key2 (cdr record))))
                (if record2 (set-cdr! record2 v)
                    (set-cdr! record (cons (cons key2 v) (cdr record)))))
            (set-cdr! table
                (cons (cons key1 (cons (cons key2 v) '())) (cdr table))))))

(define (try)
    (define a (make-table))
    (display a)
    (newline)
    (display (look-up 'k a))
    (newline)
    (insert! 'k 'v a)
    (display a)
    (newline)
    (display (look-up 'k a))
    (insert! 'k 'v1 a)
    (newline)
    (display a)
    (newline)
    (display (look-up 'k a))
    (display "test for table2")
    (define a2 (make-table2))
    (display a2)
    (newline)
    (display (look-up2 'k1 'k2 a2))
    (newline)
    (insert2! 'k1 'k2 'v a2)
    (display a2)
    (newline)
    (display (look-up2 'k1 'k2 a2))
    (newline)
    (display (look-up2 'k2 'k2 a2))
)
