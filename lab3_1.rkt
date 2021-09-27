#lang racket

; для перевірки: https://www.desmos.com/calculator?lang=ru

; Для обрахування похідної функції однієї змінної в заданій точці
; f - посилання на функцію, x0 - точка, в якій шукаємо похідну
(define (dfdx f x)
  (let* ([dx 0.0001]
         [dy (- (f (+ x dx)) (f x))])
    (/ dy dx))
)

; Функція для знаходження
; кореня рівняння f(x) = 0 методом Ньютона
; f - посилання на функцію
; x - початкова точка для пошуку
; accuracy - рівень точності для зупинки рекурсивних викликів
(define (newton-method f x [accuracy 0.0001])
  (let ([iteration-counter 0])
    
    (define (newton-method-tail)
      (set! iteration-counter (add1 iteration-counter))
      (cond
        [(< (abs (f x)) accuracy) x]
        [else
         (set! x (- x (/ (f x) (dfdx f x))))
         (newton-method-tail)]
      )
    )
    
    (cons (newton-method-tail) iteration-counter)
  )
)

; Функція для знаходження
; кореня рівняння f(x) = 0 методом бісекцій
; f - посилання на функцію
; a, b - такі межі для пошуку, що f(a)*f(b) < 0
; accuracy - рівень точності для зупинки рекурсивних викликів
(define (bisection-method f a b [accuracy 0.0001])
  (let ([iteration-counter 0]
        [mid 0])
    
    (define (bisection-method-tail a b)
      (set! iteration-counter (add1 iteration-counter))
      (set! mid (/ (+ a b) 2.0))
      (cond
        [(< (abs (f mid)) accuracy) mid]
        [(positive? (* (f mid) (f a))) (bisection-method-tail mid b)]
        [else (bisection-method-tail a mid)]
      )
    )
      
    (cons (bisection-method-tail a b) iteration-counter)
  )
)

; виклик функцій для виведення результату
; створюємо анонімну функцію f(x),
; аргумент x0 для старту пошуку методом Ньютона,
; a та b - для пошуку методом бісекцій
(let* ([f (lambda (x) (- x (* 3 (exp (* -3 x)))) )]
       [x0 100]
       [a 0]
       [b 100]
       [newton-results (newton-method f x0)]
       [bisection-results (bisection-method f a b)])
  (display "Newton method results:")
  (newline)
  (display "x: ")
  (display (car newton-results))
  (newline)
  (display "iteration count: ")
  (display (cdr newton-results))
  (newline)
  (newline)
  (display "Bisection method results:")
  (newline)
  (display "x: ")
  (display (car bisection-results))
  (newline)
  (display "iteration count: ")
  (display (cdr bisection-results))
)

