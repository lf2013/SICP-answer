; 2.19

; 1.22 old solution
(define (count-change amount)
    (cc amount 5))

(define (cc amount kinds-of-coins)
    (cond ((= 0 amount) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else
              (+
                (cc amount (- kinds-of-coins 1))
                (cc (- amount  (first-demination kinds-of-coins)) kinds-of-coins)))))

(define (first-demination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))

(define (try)
    (display (count-change 100)))

; new solution
(define (cc1 amount kinds-of-coins)
    (define (no-more? x) (null? x))
    (define (except-first-demination x) (cdr x))
    (define (first-demination x) (car x))
    (cond ((= 0 amount) 1)
          ((or (< amount 0) (no-more? kinds-of-coins)) 0)
          (else
              (+
                (cc1 amount (except-first-demination kinds-of-coins))
                (cc1 (- amount  (first-demination kinds-of-coins)) kinds-of-coins)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (try1)
    (display (cc1 100 us-coins))
    (newline)
    (display (cc1 100 uk-coins))
)
