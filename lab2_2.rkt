#lang racket

(define (square x)
  (* x x)
)

; функція для обрахування нескінченного дробу за правилом:
; 4 / (1 + 1 / (3 + 2^2 / (5 + 3^2 / (7 + ...)))
(define (calculate-fraction)
  (define error 0.01)
  
  ; визначаємо рекурсію
  ; number - перший доданок в знаменнику
  ; nominator - значення в чисельнику
  (define (calculate-fraction-recursive [number 1] [nominator 1])

    ; умова завершення рекурсивних викликів
    (cond
      [(< (abs (-
           (/
            (square nominator)
            (+ number 2 (square (+ nominator 1)))
           )
           1
          ))
          error
       )
       (+ number (square nominator))]
      [else
       (+ number
          (/
           (square nominator)
           (calculate-fraction-recursive (+ number 2) (+ nominator 1))
          )
       )]
    )
  )

  ; виклик рекурсивної функції для повернення результату
  (/ 4. (calculate-fraction-recursive))
)

(calculate-fraction)