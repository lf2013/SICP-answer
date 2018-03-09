; things worth doing typically take time and effort

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low 
                   (stream-enumerate-interval (+ 1 low) high))))
(define (stream-ref s n)
  (if (= n 0) (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams )
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (show x)
  (display x)(newline)
    x)

(define (display-line x)
	(newline) (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

; cache version
(define (try)

	(define sum 0)

	(define (accum x)
	  (set! sum (+ x sum))
	  sum)
	
	; 0 0
	; 1 1
	; 2 3
	; 3 6
	; 4 10
	; 5 15
	; 6 21
	; 7 28
	; 8 36
	; 9 45
	; 10 55
	; 11 66
	; 12 78
	; 13 91
	; 14 105
	; 15 120
	; 16 136
	; 17 153
	; 18 171
	; 19 190
	; 20 210
	; 1, 1 + 2, 1 + 2 + 3, ...
	(define seq 
	  (stream-map 
	   accum 
	   (stream-enumerate-interval 1 20)))
	
	; 6, 10, 28, 36, 66, 78, 120, 136
	(define y (stream-filter even? seq))
	
	(define z 
	  (stream-filter 
	   (lambda (x) 
	     (= (remainder x 5) 0)) seq))
	
	; 136
	(stream-ref y 7)

	; 10, 15, 45, 55, 105, 120, 190, 210
	(display-stream z)
)

(try)
