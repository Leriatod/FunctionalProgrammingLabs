#lang racket

; ф-ція для створення комплексних чисел (a + bi) із списку
; з дійсними частинами та уявними частин (довжини повинні співпадати)
; real-parts-list - список з дійсними частинами (a)
; imaginary-parts-list - список з уявними частинами (b)
(define (create-complex-list real-parts-list imaginary-parts-list)
  ; обраховуємо довжину списку
  (define n (length real-parts-list))
  ; створюємо порожній список для комплексних чисел
  (define complex-list '())
  
  (define real-part 0)
  (define imaginary-part 0)
  (define complex-number 0)
  
  ; Цикл, лічильник від 0 до n-1
  (for ([idx n])
    (set! real-part (list-ref real-parts-list idx))
    (set! imaginary-part (list-ref imaginary-parts-list idx))

    ; додаємо нове комплексне число у список (a + bi)
    (set! complex-list
          (append
            complex-list
            (list (+ real-part (* imaginary-part +i)))
          ))
  )
  ; повертаємо створений список з комплексними числами
  complex-list
)

(define (main)
  ; задаємо вхідні дані
  (define real-parts-list '(1 2  7 -5 10))
  (define imaginary-parts-list '(3 5 -1  8 3))
  (define complex-numbers (create-complex-list real-parts-list imaginary-parts-list))

  ; друк вхідних та вихідних значень
  (display "Real parts: ")
  (display real-parts-list)
  (newline)
  (display "Imaginary parts: ")
  (display imaginary-parts-list)
  (newline)
  (display "Complex numbers: ")
  (display complex-numbers)
  (newline)
  
)

(main)