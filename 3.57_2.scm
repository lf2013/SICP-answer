; things worth doing typically take time and effort

(define (force delay_op)
	; (display-line "force")
	(delay_op))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

; no cache version
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream 
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

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
		(begin (display 'x)
        (apply proc (map stream-car argstreams)))
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

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

; no cache version
(define (try)
	; n times
	; 1 2 3 4 5 6 7  n - 2  n - 1  	   n
	; 0 0 1 2 4 7		a	  b    a + b + 1
	(define fib (cons-stream 1 (cons-stream 2 (add-stream (stream-cdr fib) fib))))
	(display-stream fib)
)

(try)
