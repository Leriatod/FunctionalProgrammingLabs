#lang racket

(define (is-even n) 
  (= 0 (modulo n 2))
)

(define recursion-depth 0)

(define (pow x n)
  (define recursion-depth 0)
  
  (define (pow-tail x n [result 1])
    (set! recursion-depth (+ recursion-depth 1))
    
    (cond
      [(= n 0) result]
      [(= n 1) (* x result)]
      [(is-even n) (pow-tail (* x x) (/ n 2) result)]
      [else (pow-tail (* x x) (/ (- n 1) 2) (* x result) )]
    )
  )

  (cons (pow-tail x n) recursion-depth)
)

(pow (read) (read))


