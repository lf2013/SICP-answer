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

(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream (force integrand) dt)
                  int)))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral2
         integrand initial-value dt)
  (cons-stream 
   initial-value
	(let ((a (force integrand)))
   (if (stream-null? a)
       the-empty-stream
       (integral2
        (delay (stream-cdr a))
        (+ (* dt (stream-car a))
           initial-value)
        dt)))))

(define (solve2 f y0 dt)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (solve-2nd-general f y0 dy0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (integral (delay ddy) dy0 dt))
	(define ddy (stream-map f dy y))
	y)

(define (solve-2nd a b y0 dy0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (integral (delay ddy) dy0 dt))
	(define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
	y)

(define (solve-2nd2 a b y0 dy0 dt)
	(solve-2nd-general (lambda (x y) (+ (* a x) (* b y))) y0 dy0 dt))

(define (try)
	(newline)
	(display (stream-head (int 1) 10)) (newline)
	(display (stream-head (solve-2nd 1 1 1  1 0.001) 10)) (newline)
	(display (stream-ref (solve-2nd 1 1 1  1 0.001) 10000)) (newline)
	(display (stream-head (solve-2nd2 1 1 1  1 0.001) 10)) (newline)
	(display (stream-ref (solve-2nd2 1 1 1  1 0.001) 10000)) (newline)
)

(try)
