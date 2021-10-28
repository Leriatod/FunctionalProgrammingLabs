#lang racket

; ф-ція для створення порожньої черги
(define (make-queue) '() )

; функція, що повертає чергу з доданим елементом
(define (enqueue queue value)
  (append queue (list value))
)

; ф-ція для видалення елементу черги,
; повертає пару зі значення видаленого елементу і значенням черги без нього
(define (dequeue queue)
  (cons
    (list-tail queue 1)
    (list-ref queue 0)
  )
)

; ф-ція, що повертає чергу, яка містить лише парні числа з вхідної
(define (make-queue-with-even-numbers input-queue)
  (define queue-even (make-queue))

  (define (empty-queue)
    (cond
      [(= (length input-queue) 0) queue-even]

      [else
        (let* ([dequeue-pair (dequeue input-queue)]
               [queue (car dequeue-pair)]
               [value (cdr dequeue-pair)])
          (set! input-queue queue)

          (if (= (modulo value 2) 0)
              (set! queue-even (enqueue queue-even value))
              0
          )
          
        )
        (empty-queue)]
    )
    
  )
  
  (empty-queue)
)

(define (main)
  (define q (make-queue))
  
  (set! q (enqueue q 8))
  (set! q (enqueue q 7))
  (set! q (enqueue q 6))
  (set! q (enqueue q 5))
  (set! q (enqueue q 3))
  (set! q (enqueue q -4))
  (set! q (enqueue q 2))
  (set! q (enqueue q -2))

  (display "Input queue: ")
  (display q)
  (newline)
  (display "Output queue: ")
  (display (make-queue-with-even-numbers q))
)

(main)