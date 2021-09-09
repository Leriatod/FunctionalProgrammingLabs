#lang racket

(define (square x) ; функція піднесення числа до квадрату
  (* x x)
)

(define (is-even n) ; функція для перевірки кратності двум
  (= 0 (modulo n 2))
)

(define (pow x n) ; функція піднесення числа x до степеня n
  (cond
   [(= n 1) x] ; умова зупинки рекурсії, якщо степінь дорівнює 1, повертаємо x
   [(is-even n) ( square (pow x (/ n 2)) )]  
   [else (* x (square (pow x (/ (- n 1) 2)) ) )]
  )
)

(pow (read) (read))

(define (is-last-digit n) ; перевірка чи натуральне число складається з однієї цифри
  (< n 10)
)

(define (get-last-digit n) ; повертаємо останню цифру числа
  (modulo n 10)
)

(define (extract-last-digit n) ; повертаємо число без останньої цифри
  (floor(/ n 10))
)

(define (sum-all-digits n) ; рекурсивна функція для підрахунку суми цифр числа
  (cond
    [(is-last-digit n) n] ; якщо число складається з однієї цифри - повертаємо його
    [else (+
           (get-last-digit n) ; вилучаємо останню цифру і додаємо до іншої суми
           (sum-all-digits (extract-last-digit n) ) ; рекурсивний виклик для обрахювання 
          )]
  )
)

;(sum-all-digits (read))