#lang racket

(define (is-last-digit n) 
  (< n 10)
)

(define (get-last-digit n) 
  (modulo n 10)
)

(define (extract-last-digit n) 
  (floor(/ n 10))
)

(define (sum-all-digits n) 
  (define (sum-all-digits-tail n [sum 0])
    (cond
      [(is-last-digit n) (+ sum n)]
      [else (sum-all-digits-tail
              (extract-last-digit n)
              (+ sum (get-last-digit n))
            )]
    )
  )
  (sum-all-digits-tail n)
)

(sum-all-digits (read))