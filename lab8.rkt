#lang racket

; ф-ція для округлення дійсного числа до n цифр після коми
(define (round-off x n)
  (define power (expt 10 n))
  (/ (round (* power x)) power)
)

; ф-ція для чисельного знаходження похідної
(define (dfdx x f [dx 0.001])
  (/
   (- (f (+ x dx)) (f x))
   dx)
)

; ф-ція для знаходження коренів рівняння f(x) = 0 методом Ньютона
(define (newton-solve x f [accuracy 0.00001] [iteration-number 100])
  (define (solve [iteration-counter 0])
    (cond
      ; якщо максимальну к-ть ітерацій пройдено - корінь не знайдено, повертаємо false
      [(= iteration-counter iteration-number) #f]
      [(< (abs (f x)) accuracy) x]
      [else
       (set! x (- x (/ (f x) (dfdx x f))))
       (solve (add1 iteration-counter))]
    )
  )
  
  (solve)
)

; ф-ція для знаходження значення поліному вигляду An * x^(n) + An-1 * x^(n-1) + ... + A0
(define (get-polinomial-value-by-coeff-at x0 coeff-list)
  (define n (length coeff-list))
  (define result 0)
  
  (for ([i n])
    (set!
     result
     (+ result (* (list-ref coeff-list i) (expt x0 (- n i 1)) ) )
    )
  )
    
  result
)

; ф-ція для знаходження значення поліному вигляду (x - root1)*(x - root2)*...*(x - rootn) 
(define (get-polinomial-value-by-roots-at x0 roots-list)
  (define n (length roots-list))
  (define result 1)
  
  (for ([i n])
    (set!
     result
     (* result (- x0 (list-ref roots-list i) ) )
    )
  )
    
  result
)

; ф-ція для знаходження всіх дійсних коренів поліноміального рівняння 
(define (find-polinomial-roots coeff-list)
  ; хвостова рекурсія для визначення коренів за допомогою методу Ньютона
  (define (find-roots polinomial [polinomial-roots '()] [x0 -100])
    (define root (newton-solve x0 polinomial))
    
    (cond
      ; якщо новий корінь знайдено не було - повертаємо список знайдених коренів
      [(not root) polinomial-roots]
      [else
       (set! polinomial-roots (append polinomial-roots (list root)))
       
       (find-roots
        ; після знаходження нового кореня задаємо нове значення для ф-ції полінома: P(x) = P(x) / (x - root)
        (lambda (x) (/ (get-polinomial-value-by-coeff-at x coeff-list)
                       (get-polinomial-value-by-roots-at x polinomial-roots)))
        polinomial-roots)]
    )
  )
  
  (find-roots (lambda (x) (get-polinomial-value-by-coeff-at x coeff-list)))
)

; ф-ція для запису коренів поліному до текстового файлу
(define (write-roots-to-file roots file-name)
  (define n (length roots))

  (define out (open-output-file file-name #:exists 'replace))

  (for ([i n])
    (display "x" out)
    (display i out)
    (display " = " out)
    (displayln (round-off (list-ref roots i) 4) out)
  )
  
  (close-output-port out)
)


(define (main)
  (define coeff-list '(1 -3 -4 12))
  (define n (length coeff-list))

  (displayln "Find equation roots: ")

  ; друкуємо поліноміальне рівняння із заданого списку коефіцієнтів
  (for ([i n])
    (display (list-ref coeff-list i))

    (when (< i (- n 1))
      (display "*x^(")
      (display (- n i 1))
      (display ")")
      (when (> (list-ref coeff-list (add1 i)) 0)
          (display "+"))
    )
  )
  (displayln " = 0")

  
  (define roots (find-polinomial-roots coeff-list))


  ; Друкуємо знайдені корені 
  (newline)
  (displayln "Roots: ")
  (for ([i (length roots)])
    (display "\tx")
    (display (add1 i))
    (display " = ")
    (displayln (round-off (list-ref roots i) 2))
  )

  ; За знайденими коренями розкладаємо поліном на множники і друкуємо
  (newline)
  (displayln "Equivalent to: ")
  (display "\t")
  (for ([i (length roots)])
    (define root (list-ref roots i))
    (display "(x")
    (when (> root 0)
      (display "+"))
    (display (round-off root 2))
    (display ")")
  )
  (displayln " = 0")


  ; Записуємо корені поліному до текстового файлу 
  (write-roots-to-file roots "roots.txt")
)

(main)