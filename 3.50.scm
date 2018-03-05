; things worth doing typically take time and effort

(define (cons-stream a b)
	(cons a (delay b)))
(define (car-stream a)
	(car a))
(define (cdr-stream a)
	(force (cdr a)))

(define (make-stream a b)
	(cond ((< a b)
		(cons-stream a (make-stream (+ a 1) b)))
		(else '())))

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

(define (stream-map pred stream)
  (cond ((stream-null? stream) 
         '())
         (else (cons-stream 
          (pred (stream-car stream))
          (stream-map
           pred
           (stream-cdr stream))))))

(define (stream-map2 proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc 
                    (map stream-cdr
                         argstreams))))))

(define (stream-display stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
         (else (newline) (display (car-stream stream))
          (stream-display
           (stream-cdr stream)))))

(define (try)
	(define a (make-stream 1 10))
	(newline)
	(stream-display  a)
	(newline)
	(stream-display (stream-map2 (lambda (a) (* a a)) a))
	(newline)
	(define b (make-stream 1 10))
	(define c (make-stream 1 10))
	(stream-display (stream-map2 + c b b))
	(newline)
)

(try)
