#lang racket

; ф-ція для друку списку на екрані,
; кожне значення у списку друкуємо з нового рядка
(define (display-list lst)
  (define n (length lst))

  ; для кожного значення у списку
  (for ([i n])
    (display (list-ref lst i))
    (newline)
  )
)

; ф-ція для перетворення кожного рядка списку у верхній регістр,
; якщо перша літера цього рядка починається з великої літери.
; Повертає новий список рядків, утворений за даним правилом.
(define (to-upcase-strings-if-starts-with-upchar list-of-strings)
  (define n (length list-of-strings))
  (define new-list '())

  ; для кожного рядка у списку
  (for ([i n])
    (define str (list-ref list-of-strings i))
    ; якщо перша літера у верхньому регістрі
    (when (char-upper-case? (string-ref str 0))
      ; переводимо поточний рядок у верхній регістр
      (set! str (string-upcase str))
    )

    ; додаємо поточний рядок у новий список
    (set! new-list (append new-list (list str)))
  )

  ; повертаємо новий список
  new-list
)

; ф-ція для читання рядків з файлу
; повертає список рядків файлу
(define (read-lines-from-file file-name)
  (define lines-list '())

  ; відкриваємо файл для читання
  (define in (open-input-file file-name))

  ; визначаємо хвостову рекурсію
  (define (read-lines-to-end)
    ; читаємо рядок з файлу
    (define line (read-line in))
    (cond
      ; якщо файл прочитано до кінця
      [(eof-object? line)
       ; закриваємо файл для читання
       (close-input-port in)
       ; повертаємо список рядків
       lines-list]
      [else
       ; додаємо прочитаний рядок до списку
       (set! lines-list (append lines-list (list line)))
       ; рекурсивний виклик ф-ції
       (read-lines-to-end)]
    ) 
  )

  (read-lines-to-end)
)

; ф-ція для запису списку рядків у файл
(define (write-list-of-strings-to-file list-of-strings file-name)
  (define n (length list-of-strings))

  ; відкриваємо файл для запису
  (define out (open-output-file file-name #:exists 'replace))

  ; для кожного рядку у списку
  (for ([i n])
    ; записуємо рядок у файл
    (display (list-ref list-of-strings i) out)
    ; починаємо запис з нового рядка
    (newline out)
  )

  ; закриваємо файл для запису
  (close-output-port out)
)


(define (main)
  (display "Виберіть скільки рядків хочете записати: ")

  ; вводимо к-ть рядків для введення
  (define n (read))
  (define list-of-strings '())
  
  (read-line)

  ; вводимо кожний рядок
  (for ([i n])
    (display "Введіть рядок № ")
    (display (add1 i))
    (display ": ")

    ; додаємо рядок до списку після введення
    (set! list-of-strings
          (append list-of-strings (list (read-line))))
  )

  ; запис списку рядків до файлу
  (write-list-of-strings-to-file list-of-strings "file1.txt")
  
  (newline)
  (display "Lines read from the first file: ")
  (newline)

  ; читаємо список рядків з файлу
  (set! list-of-strings (read-lines-from-file "file1.txt"))
  ; друк списку рядків на екран
  (display-list list-of-strings)

  ; переводимо рядки списку у верхній регістр, якщо перший символ є великою літерою
  (set! list-of-strings
        (to-upcase-strings-if-starts-with-upchar list-of-strings))

  ; записуємо список рядків після перетворення у інший файл
  (write-list-of-strings-to-file list-of-strings "file2.txt")

  (newline)
  (display "Lines read from the second file: ")
  (newline)
  ; читаємо список-рядків з новоутвореного файлу, друкуємо на екрані
  (display-list (read-lines-from-file "file2.txt"))
)

(main)