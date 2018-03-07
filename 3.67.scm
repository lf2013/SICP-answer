
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

(define (add-streams s1 s2)
	(stream-map + s1 s2))

(define (mul-stream s1 s2)
	(stream-map * s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
				(add-streams (scale-stream (stream-cdr s2) (stream-car s1)) 
							 (mul-series (stream-cdr s1) s2))))

(define (int n) (cons-stream n (int (+ 1 n))))

(define (integrate-series s) (stream-map / s (int 1)))

(define (average a b)
	(/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (ln2-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define (add-stream s1 s2)
	(stream-map + s1 s2))

(define (partial-sums s) 
	(define a
		(cons-stream (stream-car s) (add-stream a (stream-cdr s))))
	a)

(define ln2-stream
   (partial-sums (ln2-summands 1)) )

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
	(cons-stream s
		(make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
	(stream-map stream-car (make-tableau transform s)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
	(interleave 
    	(stream-map (lambda (x) 
    	              (list x (stream-car t) ))
    	            (stream-cdr s))
    	(pairs (stream-cdr s) (stream-cdr t))))))

(define (try)
	(newline)
	(display (stream-head (int 1) 40)) (newline)
	(display (stream-head (pairs (int 1) (int 1)) 20)) (newline)
	; (2 ** i) (j - i) + (2 ** (i - 1) - 1) 
	(display (stream-head (stream-map (lambda (a b) (list a b)) (int 1) (pairs (int 1) (int 1))) 140)) (newline)
)

(try)
