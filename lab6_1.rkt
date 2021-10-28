#lang racket

; ф-ція для перевірки числа на парність
(define (is-even n)
  (= (modulo n 2) 0)
)

; ф-ція для перевірки числа на кратність 3
(define (is-divisible-by-three n)
  (= (modulo n 3) 0)
)

; ф-ція для перевірки числа на кратність 6
(define (is-divisible-by-six n)
  (= (modulo n 6) 0)
)

; ф-ція для додавання числа в кінець вектора
(define (vector-add v number)
  (vector-append v (vector number))
)

; ф-ція, що створює новий вектор з вхідного, який складається спочатку  
; з елементів, що діляться тільки на 2
; потім з елементів, що діляться на 2 і на 3
; потім з елементів, що діляться тільки на 3
(define (make-vector-by-rule-263 vector)
  (define n (vector-length vector))
  (define divisible-by-two-only-numbers #())
  (define divisible-by-six-numbers #())
  (define divisible-by-three-only-numbers #())

  (for ([index n])
    (define number (vector-ref vector index))

    (cond
      ; якщо кратне шести - додаємо у вектор кратних 6
      [(is-divisible-by-six number)
       (set! divisible-by-six-numbers
             (vector-add divisible-by-six-numbers number))]
      ; якщо кратне тільки двум - додаємо у вектор парних
      [(is-even number)
       (set! divisible-by-two-only-numbers
             (vector-add divisible-by-two-only-numbers number))]
      ; якщо кратне тільки трьом - додаємо у вектор кратних 3
      [(is-divisible-by-three number)
       (set! divisible-by-three-only-numbers
             (vector-add divisible-by-three-only-numbers number))]
    )
  )

  ; Об'єднуємо вектори для повернення результату
  (vector-append
    divisible-by-two-only-numbers
    divisible-by-six-numbers
    divisible-by-three-only-numbers)
)

(define (main)
  (define v1 #(2 6 5 7 8 9 12 10))
  (define v2 (make-vector-by-rule-263 v1))

  (display "Input vector: ")
  (display v1)
  (newline)
  (display "Output vector: ")
  (display v2)
  (newline)
)

(main)