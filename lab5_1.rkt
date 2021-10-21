#lang racket

; створення пари із чисельника і знаменника
(define (make-fraction nominator denominator)
  (cons nominator denominator)
)

; виконуємо віднімання дробів, заданих у парі,
; повертає пару з чисельником і знаменником
(define (subtract-fraction x-fraction y-fraction)
  (define n1 (car x-fraction))
  (define n2 (car y-fraction))
  (define d1 (cdr x-fraction))
  (define d2 (cdr y-fraction))
  
  (make-fraction
   (- (* n1 d2) (* n2 d1))
   (* d1 d2)
  )
)

; виконуємо ділення дробів, заданих у парі,
; повертає пару з чисельником і знаменником
(define (divide-number-by-fraction number fraction)
  (define nominator (car fraction))
  (define denominator (cdr fraction))
  
  (/ (* number denominator) nominator)
)

; ф-ція для вирішення наступної задачі:
; сума часток x y z дорівнює 1
; відоме значення часток для x та y та сума грошей для z
; потрібно знайти загальне значення суми
;
; x-fraction - частка грошей для x
; y-fraction - частка грошей для y
; z-sum - сума грошей для z
; повертає загальну суму грошей
(define (find-total-sum x-fraction y-fraction z-sum)
  (define z-fraction (make-fraction 1 1))
    
  (set! z-fraction (subtract-fraction z-fraction x-fraction))
  (set! z-fraction (subtract-fraction z-fraction y-fraction))

  (divide-number-by-fraction z-sum z-fraction)
)

(define (main)
  ; Задаємо умову задачі
  (define x-fraction (make-fraction 1 4))
  (define y-fraction (make-fraction 1 7))
  (define z-sum 17)

  ; Друк вхідних умов та результату
  (display "x fraction: ")
  (display x-fraction)
  (newline)
  (display "y fraction: ")
  (display y-fraction)
  (newline)
  (display "z sum: ")
  (display z-sum)
  (newline)
  (newline)
  (display "Total sum: ")
  (display (find-total-sum x-fraction y-fraction z-sum))
)

(main)