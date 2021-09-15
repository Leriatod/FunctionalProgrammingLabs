#lang racket

(define (sine x)
  (define error 0.000001)
  (define x-square (* x x))
  
  (define cur-taylor-el x)
  (define curSum x)
  (define n 1)
  
  
  (define (sin-tail)
    (set!
      cur-taylor-el
      (/ (* cur-taylor-el -1 x-square ) (+ n 1) (+ n 2))
    )
    (set! curSum (+ curSum cur-taylor-el))
    (set! n (+ n 2))
    
    (cond
      [(< (abs (- curSum (sin x))) error) curSum]
      [else (sin-tail)]
    )
  )

  (sin-tail)
)

(define (y x)
  (cond
    [(and (< 0 x) (< x 1)) (- (sine x) (sine (/ x 2)) )]
    [(and (<= -2 x) (<= x 0)) (- (expt (sine x) 3) (sine (+ x 0.125)))]
  )
)

(define (y-range x-start x-end step)
  (define (y-range-tail x-start x-end step [y-list '()])
    (cond
      [(> x-start x-end) y-list]
      [else (y-range-tail
             (+ x-start step)
             x-end
             step
             (append y-list (list (y x-start))) )]
    )
  )
  
  (y-range-tail x-start x-end step)
)


(y-range -2 2 0.5)