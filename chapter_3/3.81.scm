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
		; (begin (display 'x)
        (apply proc (map stream-car argstreams))
		; )
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

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (add-streams s1 s2)
	(stream-map + s1 s2))

(define (mul-stream s1 s2)
	(stream-map * s1 s2))

(define (int n) (cons-stream n (int (+ 1 n))))

(define (rand-update x)
    (remainder (+ (* 11 x) 13) 10001))

(define (gcd a b)
    (if (= 0 (remainder a b))
        b
    (gcd b (remainder a b))))
(define rand-init 1)
(define random-numbers
	(cons-stream rand-init
		(stream-map rand-update random-numbers)))
(define (map-successive-pairs f s)
	(let ((a (stream-car s))
		  (b (stream-car (stream-cdr s))))
		(cons-stream (f a b)
			(map-successive-pairs f (stream-cdr (stream-cdr s))))))
(define cesaro-stream
	(map-successive-pairs
		(lambda (r1 r2) (= (gcd r1 r2) 1))
		random-numbers))
(define (monte-carlo experiment-stream 
                     passed 
                     failed)
	(define (next passed failed)
		(cons-stream
			(/ passed (+ passed failed))
			(monte-carlo (stream-cdr experiment-stream)
				passed failed)))
		(if (stream-car experiment-stream)
			(next (+ passed 1) failed)
			(next passed (+ failed 1))))
(define pi
	(stream-map (lambda (p) (sqrt (/ 6 p)))
		(monte-carlo cesaro-stream 0 0)))
; 0 for update
; other values for reset to that value
(define request (cons-stream 1 (cons-stream 0 (cons-stream 1 (cons-stream 0 (cons-stream 1 (cons-stream 0 (int 1))))))))
(define (random-numbers2 request) 
	(define rand-s
		(cons-stream rand-init
			(stream-map (lambda (a b) (if (= 0 a) (rand-update b) a)) request rand-s)))
	rand-s)
			
(define (try)
	(newline)
	(display (stream-head random-numbers 10)) (newline)
	(display (stream-head (random-numbers2 request) 10)) (newline)
)

(try)
