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

(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (int n) (cons-stream n (int (+ 1 n))))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (sign-change-detector new old) 
	(cond
		((and (>= new 0) (>= old 0)) 0)
		((and (< new 0) (< old 0)) 0)
		((and (>= new 0) (< old 0)) 1)
		((and (< new 0) (>= old 0)) -1)))

(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (stream-car input-stream) 
    last-value)
   (make-zero-crossings 
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define (make-zero-crossings2
         input-stream last-value last-vag)
  (let ((avpt 
         (/ (+ (stream-car input-stream) 
               last-value)
            2)))
    (cons-stream 
     (sign-change-detector avpt last-vag)
     (make-zero-crossings2 
      (stream-cdr input-stream) (stream-car input-stream) avpt))))

(define sense-data (cons-stream -1 (cons-stream 0 (cons-stream -1 (cons-stream 1 (int 1))))))
(define zero-crossings 
  (make-zero-crossings sense-data 0))

(define zero-crossings2
  (stream-map sign-change-detector 
              sense-data 
              (cons-stream 0 sense-data)))

(define zero-crossings3
  (make-zero-crossings2 sense-data 0 0))

(define (try)
	(newline)
	(display (stream-head (int 1) 10)) (newline)
	(display (stream-head zero-crossings 10)) (newline)
	(display (stream-head zero-crossings2 10)) (newline)
	(display (stream-head zero-crossings3 10)) (newline)
)

(try)
