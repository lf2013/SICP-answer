; things worth doing typically take time and effort

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (force delay_op)
	; (display-line "force")
	(delay_op))

; (define (cons-stream a b)
; 	; (display-line a)
; 	; (display-line b)
; 	(cons a (delay b)))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

; cache version
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream 
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-null? s)
	(null? s))

(define (stream-map proc s)
  ; (display-line "map")
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))
       ; (begin (display-line 111111111) (proc (stream-car s)))
       ; (begin (display-line 222222222) (stream-map proc (stream-cdr s))))))

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

(define (show x)
	(display-line x) x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

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
	(display (stream-ref y 7))(newline)

	; 10, 15, 45, 55, 105, 120, 190, 210
	(display-stream z)
)

(try)
