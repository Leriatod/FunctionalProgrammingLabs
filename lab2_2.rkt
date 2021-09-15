#lang racket

(define (calculate-fraction [depth 100])
  (define (calculate-fraction-recursive [number 1] [nominator 1])
    (cond
      [(>= nominator depth) (+ number (* nominator nominator))]
      [else (+ number (/ (* nominator nominator) (calculate-fraction-recursive (+ number 2) (+ nominator 1)) ))]
    )
  )

  (/ 4. (calculate-fraction-recursive))
)

(calculate-fraction 100)