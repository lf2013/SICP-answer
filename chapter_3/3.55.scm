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

(define (add-stream s1 s2)
	(stream-map + s1 s2))

(define (partial-sums s) 
	(define a
		(cons-stream (stream-car s) (add-stream a (stream-cdr s))))
	a)

; cache version
(define (try)
	(define (int n) (cons-stream n (int (+ 1 n))))
	; (define s (cons-stream 1 (add-stream s (int 2))))
	(display (stream-head (int 1) 10)) (newline)
	(define s (partial-sums (int 1)))
	(display (stream-head s 10)) (newline)
)

(try)
