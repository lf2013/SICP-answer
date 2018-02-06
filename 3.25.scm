; things worth doing typically take time and effort

(define (make-table) (list 'table))

(define (look-up table keys) 
	(cond ((or (null? keys) (not (pair? keys)) (null? table) (not (pair? table))) false)
    	  ((null? (cdr keys))
    	      (let ((record (assoc (car keys) (cdr table))))
				(if record (cdr record) false)))
		  (else
    		(let ((record (assoc (car keys) (cdr table))))
    		    (if record (look-up record (cdr keys) )
    		        false)))))

(define (insert! table v  keys) 
	(cond ((or (null? keys) (not (pair? keys)) (null? table) (not (pair? table))) false)
	     ((null? (cdr keys)) 
			(let ((record (assoc (car keys) (cdr table))))
	         (if record (set-cdr! record v)
	             (set-cdr! table
                     (cons (cons (car keys) v) (cdr table))))))
		  (else
    		(let ((record (assoc (car keys) (cdr table))))
    		    (if record (insert! record v (cdr keys))
					(let ((new-record (cons (car keys) '())))
	             		(set-cdr! table (cons new-record (cdr table)))
						(insert! new-record v (cdr keys))))))))


(define (try)
    (newline)
    (define a (make-table))
    (display a)
    (newline)
    (display (look-up a '(k)))
    (insert! a 'v '(k))
    (newline)
    (display a)
    (newline)
    (display (look-up a '(k)))
    (newline)
    (display (look-up a '(k2 k3)))
    (newline)
    (insert! a 'v '(k2 k3 k4 k5))
    (newline)
    (display a)
    (newline)
    (display (look-up a '(k2 k3 k4 k5)))
    (newline)
    (display (look-up a '(k2 k3 k4 k6)))
    (newline)
    (insert! a 'v6 '(k2 k3 k4 k6))
    (newline)
    (display (look-up a '(k2 k3 k4 k6)))
    (newline)
    (insert! a 'v7 '(k2 k3 k4 k6))
    (newline)
    (display (look-up a '(k2 k3 k4 k6)))
)

(try)
