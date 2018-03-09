
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

(define (show x)
  (display x)(newline)
    x)

(define (try)
    (define x (stream-map show (stream-enumerate-interval 0 10)))
	; with cache version
	; 0
	; 1
	; 2
	; 3
	; 4
	; 5
    (display (stream-ref x 5))(newline)

    ; 6 
    ; 7
    (display (stream-ref x 7))(newline)
)

(try)
