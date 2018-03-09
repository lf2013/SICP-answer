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

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream 
                   s1car 
                   (merge-weighted (stream-cdr s1) 
                          s2 weight)))
                 ; ((> (weight s1car) (weight s2car))
				 (else 
                  (cons-stream 
                   s2car 
                   (merge-weighted s1 
                          (stream-cdr s2) weight))))))))
                 ; (else
                 ;  (cons-stream 
                 ;   s1car
                 ;   (merge-weighted 
                 ;    (stream-cdr s1)
                 ;    (stream-cdr s2) weight))))))))

(define (add-streams s1 s2)
	(stream-map + s1 s2))

(define (mul-stream s1 s2)
	(stream-map * s1 s2))

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
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

(define (int n) (cons-stream n (int (+ 1 n))))

(define (divide? a b) (= (modulo a b) 0))
(define (try)
	(newline)
	(display (stream-head (int 1) 40)) (newline)
	(display (stream-head (weighted-pairs (int 1) (int 1) (lambda (a) (+ (car a) (cadr a)))) 20)) (newline)
	(newline)
	(display (stream-head (stream-filter 
							(lambda (a) (let ((i (car a)) (j (cadr a))) (not (or (divide? i 2) (divide? i 3) (divide? i 5) (divide? j 2) (divide? j 3) (divide? j 5)))))
									(weighted-pairs (int 1) (int 1) 
										(lambda (a) (let ((i (car a)) (j (cadr a))) (+ (* 2 i) (* 3 j) (* 5 i j)))))) 20))
	; (display (stream-head (stream-map (lambda (a b) (list a b)) (int 1) (pairs (int 1) (int 1))) 140)) (newline)
)

(try)
