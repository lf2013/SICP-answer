; things worth doing typically take time and effort

(define (make-table samke-key?)
  (define (assoc key records) 
    (cond ((null? records) false)
		  ((samke-key? key (caar records)) (car records))
		  (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define (look-up k1 k2 t) ((t 'lookup-proc) k1 k2))

(define (insert! k1 k2 v t) ((t 'insert-proc!) k1 k2 v))

(define (try)
	(define (myequal? a b) (< (abs (- a b)) 2))
    (define a (make-table myequal?))
    (display a)
    (newline)
    (display (look-up 1 1 a))
    (newline)
    (insert! 1 1 'v a)
    (display a)
    (newline)
    (display (look-up 1 1 a))
    (newline)
    (display (look-up 2 1 a))
    (insert! 1 1 'v1 a)
    (newline)
    (display a)
    (newline)
    (display (look-up 1 1 a))
)

(try)
