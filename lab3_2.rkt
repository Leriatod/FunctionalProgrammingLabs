#lang racket

; для перевірки: https://www.wolframalpha.com/widgets/view.jsp?id=1fc8c2a70cd315e3066c37c09891d96cавппав

; Хвостова рекурсія для обчислення площі
; криволінійної трапеції методом лівих прямокутників
; f - посилання на функцію
; a, b - межі інтегрування
; dx - крок інтегрування
; sum - поточне значення суми площ прямокутників
(define (left-rectangle-sum f a b dx [sum 0])
  (cond
    [(>= a b) sum]
    [else (left-rectangle-sum f (+ a dx) b dx (+ sum (* dx (f a))))]
  )
)

; Хвостова рекурсія для обчислення площі
; криволінійної трапеції методом правих прямокутників
(define (right-rectangle-sum f a b dx [sum 0])
  (cond
    [(> a b) sum]
    [else
     (set! a (+ a dx))
     (right-rectangle-sum f a b dx (+ sum (* dx (f a))))]
  )
)

; Хвостова рекурсія для обчислення площі
; криволінійної трапеції методом середніх прямокутників
(define (mid-rectangle-sum f a b dx [sum 0])
  (cond
    [(> a b) sum]
    [else (let ([trapezoid-area (* dx (+ (f a) (f (+ a dx))) 0.5)])
            (mid-rectangle-sum f (+ a dx) b dx (+ sum trapezoid-area))
          )]
  )
)

; Хвостова рекурсія для обчислення площі
; криволінійної трапеції методом Симсона
; f - посилання на функцію
; a, b - межі інтегрування
; dx - крок інтегрування
; sum - поточне значення суми площ частин, апроксимованих квадратичною функцією
(define (simson-sum f a b dx [sum 0])
  (cond
    [(> a b) sum]
    [else
     (let ([trapezoid-area
            (*
             (/ dx 3)
             (+ (f a) (* 4 (f (+ a dx))) (f (+ a dx dx))))
           ])
       (simson-sum f (+ a dx dx) b dx (+ sum trapezoid-area))
     )]
  )
)

(let ([f (lambda (x) (cos (+ (* x x) x 1)))]
      [a 0]
      [b 1]
      [dx 0.01])
  (display "Left rectangle method: ")
  (display (left-rectangle-sum f a b dx))
  (newline)
  (display "Right rectangle method: ")
  (display (right-rectangle-sum f a b dx))
  (newline)
  (display "Mid rectangle method: ")
  (display (mid-rectangle-sum f a b dx))
  (newline)
  (display "Simson method: ")
  (display (simson-sum f a b dx))
)
