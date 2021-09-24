#lang racket

; calculating sin value by using taylor series
(define (sine x)
  (define error 0.000001)
  (define x-square (* x x))

  (define prev-taylor-el 0)
  (define cur-taylor-el x)
  (define curSum x)
  (define n 1)
  
  ; tail-recursive function for sin calculation
  (define (sin-tail)
    (set! prev-taylor-el cur-taylor-el)
    (set!
      cur-taylor-el
      (/ (* cur-taylor-el -1 x-square ) (+ n 1) (+ n 2))
    )
    (set! curSum (+ curSum cur-taylor-el))
    (set! n (+ n 2))

    ; if |cur taylor element - prev taylor element| < error
    ; return cur sum of taylor series
    (cond
      [(< (abs (- cur-taylor-el prev-taylor-el)) error) curSum]
      [else (sin-tail)]
    )
  )

  (sin-tail)
)

; returns sine(x)-sine(x/2) for 0<x<1
; and sine(x)^3 - sine(x + 0.125) for -2<=x<=0
(define (y1 x)
  (cond
    [(and (< 0 x) (< x 1)) (- (sine x) (sine (/ x 2)) )]
    [(and (<= -2 x) (<= x 0)) (- (expt (sine x) 3) (sine (+ x 0.125)))]
  )
)

; the same as y1, but with libary sin
(define (y2 x)
  (cond
    [(and (< 0 x) (< x 1)) (- (sin x) (sin (/ x 2)) )]
    [(and (<= -2 x) (<= x 0)) (- (expt (sin x) 3) (sin (+ x 0.125)))]
  )
)

; returns list of y values from x-start to x-end (included)
; with defined step
(define (y-range x-start x-end step y)
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

; printing y values for custom sin function
(y-range -2 2 0.5 y1)

; printing y values for library sin function
(y-range -2 2 0.5 y2) 