#lang racket

; Функція для підрахунку суми чисел у списку з заданого індексу
; lst - список
; start-index - індекс з якого починаємо рахувати
(define (sum-from lst [start-index 0])
  (define (sum-tail [sum 0] [index start-index])
    (cond
      [(>= index (length lst)) sum]
      [else (sum-tail
             (+ sum (list-ref lst index))
             (add1 index))]
    )
  )
  (sum-tail)
)

; Функція для генерації випадкового цілого числа в межах
; min - мінімальне значення
; max - максимальне значення
(define (generate-random-between min max)
  (+ min (exact-floor (* (random) (- max min))))
)

; Функція для вставки випадкових елементів в кінець списку
; Створює новий список з поточного
; lst - список
; min, max - діапазон генерації випадкових чисел
; n - к-ть нових випадкових чисел
(define (push-random-numbers-between-range lst min max n)
  (define new-lst lst)
  
  (for ([i n])
    (set! new-lst
          (append new-lst (list (generate-random-between min max))))
  )
  
  new-lst
)

; Функція для знаходження мінімального елементу
; у списку разом з його індексом
(define (find-min lst)
  ; Хвостова рекурсія для знаходження мінімального елементу та індексу
  ; Повертає пару (мін. ел. мін. індекс)
  (define (find-min-index-tail min [cur-index 0] [min-index 0])
    (cond
      [(>= cur-index (length lst)) (cons min min-index)]
      [(< (list-ref lst cur-index) min)
       (find-min-index-tail (list-ref lst cur-index) (add1 cur-index) cur-index)]
      [else (find-min-index-tail min (add1 cur-index) min-index)]
    )
  )

  ; Якщо список порожній повертаємо -1, ні - виклик хвостової рекурсії
  (if (empty? lst) -1
      (find-min-index-tail (first lst)))
)

; Функція знаходження максимального елементу у списку та його індексу
(define (find-max lst)
  (define (find-max-index-tail max [cur-index 0] [max-index 0])
    (cond
      [(>= cur-index (length lst)) (cons max max-index)]
      [(> (list-ref lst cur-index) max)
       (find-max-index-tail (list-ref lst cur-index) (add1 cur-index) cur-index)]
      [else (find-max-index-tail max (add1 cur-index) max-index)]
    )
  )

  (if (empty? lst) -1
      (find-max-index-tail (first lst)))
)

; Створює список факторіалів натурального числа n (від 1 до n включаючи)
(define (create-factorials-list n)
  (define factorials-lst '())
  (define factorial 1)

  (for ([i n])
    (set! factorial (* factorial (add1 i)))
    (set! factorials-lst
          (append factorials-lst (list factorial)))
  )

  factorials-lst
)

(define (main)
  (define cur-list-length 5)
  (define new-elements-number 3)
  (define factorials-list (create-factorials-list cur-list-length))
  (define max-pair (find-max factorials-list))
  (define min-pair (find-min factorials-list))
  
  (display "List of factorials: " )
  (display factorials-list)
  (newline)
  (display "Max value and index: ")
  (display max-pair)
  (newline)
  (display "Min value and index: ")
  (display min-pair)
  (newline)
  
  (set! factorials-list
        (push-random-numbers-between-range
         factorials-list
         (car min-pair)
         (car max-pair)
         new-elements-number))

  (display "List after insertion: ")
  (display factorials-list)
  (newline)
  (display "Sum of added numbers: ")
  (display (sum-from factorials-list cur-list-length))
  (newline)
)

(main)

