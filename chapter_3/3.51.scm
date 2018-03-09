; things worth doing typically take time and effort

; (define (cons-stream a b)
; 	(cons a (delay b)))
; 
; (define (car-stream a)
; 	(car a))
; (define (cdr-stream a)
; 	(force (cdr a)))
; 
; (define (make-stream a b)
; 	(cond ((< a b)
; 		(cons-stream a (make-stream (+ a 1) b)))
; 		(else '())))

; (define (stream-filter pred stream)
;   (cond ((stream-null? stream) 
;          the-empty-stream)
;         ((pred (stream-car stream))
;          (cons-stream 
;           (stream-car stream)
;           (stream-filter 
;            pred
;            (stream-cdr stream))))
;         (else (stream-filter 
;                pred 
;                (stream-cdr stream)))))

; (define (stream-map pred stream)
;   (cond ((stream-null? stream) 
;          '())
;          (else (cons-stream 
;           (pred (stream-car stream))
;           (stream-map
;            pred
;            (stream-cdr stream))))))

; (define (stream-map2 proc . argstreams)
;   (if (null? (car argstreams))
;       the-empty-stream
;       (cons-stream
;        (apply proc (map stream-car argstreams))
;        (apply stream-map2
;               (cons proc 
;                     (map stream-cdr
;                          argstreams))))))

; (define (delay delay_op)
; 	(display-line "delay")
; 	(lambda () delay_op))

;;;;;;


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

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

; no cache version
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

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

(define (display-line x)
	(newline) (display x))

(define (show x)
	(display-line x) x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (try)
	; (define x (stream-map show (stream-enumerate-interval 1 10)))
	; (display-line (stream-enumerate-interval 0 10))
	(define x (stream-enumerate-interval 0 10))
	; (display-line x)
	; (display-line (stream-car x))
	; (display-line (stream-cdr x))
	; (define y (stream-map show x))
	(define y (stream-map show x))
	; (display-line y)
	; (display-line (stream-car y))
	; (display-line (stream-cdr y))
	; (display-line (stream-cdr (stream-cdr y)))
	; (display-line (stream-map (lambda (a) (begin (display-line a) (* a a))) (stream-enumerate-interval 0 10)))

	; no cache version
	; 0
	; 1
	; 2
	; 3
	; 4
	; 5
	(display-line (stream-ref y 5))(newline)

	; 0
	; 1
	; 2
	; 3
	; 4
	; 5
	; 6
	; 7
	(display-line (stream-ref y 7))(newline)
)

(try)
