#lang racket

; Функція, що розбиває пацієнтів на наступні категорії:
;    - пацієнти, підозрювані на ковід
;    - пацієнти, що контактували з хворими на ковід
;    - пацієнти з важким ковідом, відправлені до лікарні
;    - не обслуговані пацієнти
; повертає список списків з данними категорій
; patients - список з данними пацієнтів
; doctor-time - час (в хв.), що може обслуговувати лікар
; patient-time - час обслуговування пацієнта
(define (serve-patients patients doctor-time patient-time)
  (define covid-suspect-patients '())
  (define covid-contact-patients '())
  (define covid-hard-patients '())
  (define unserved-patients '())
  (define patient-status "")
  (define patient-name "")
  (define patient '())
  (define index 0)
  
  (define (serve-patients-tail)
    (set! patient (list-ref patients index))
    (set! patient-status (list-ref patient 1))
    (set! patient-name (list-ref patient 0))
    
    (cond
      [(< doctor-time patient-time)
       (set! unserved-patients (append unserved-patients (list patient-name)))]
      [(equal?  "covid contact" patient-status)
       (set! covid-contact-patients (append covid-contact-patients (list patient-name)))]
      [(equal?  "covid suspect" patient-status)
       (set! covid-suspect-patients (append covid-suspect-patients (list patient-name)))]
      [(equal?  "covid hard" patient-status)
       (set! covid-hard-patients (append covid-hard-patients (list patient-name)))]
      [else 0]
    )
    (set! doctor-time (- doctor-time patient-time))
    (set! index (add1 index))

    (if (>= index (length patients))
      (list
         covid-contact-patients
         covid-suspect-patients
         covid-hard-patients         
         unserved-patients)
      (serve-patients-tail))  
  )
  
  (serve-patients-tail)
)

(define (main)
  (define doctor-time 480)
  (define patient-time 40)

  (define patients
    (list
     (list "patient 1" "covid suspect")
     (list "patient 2" "covid hard")
     (list "patient 3" "covid no suspect")
     (list "patient 4" "covid contact")
     (list "patient 5" "covid no suspect")
     (list "patient 6" "covid suspect")
     (list "patient 7" "covid no suspect")
     (list "patient 8" "covid contact")
     (list "patient 9" "covid hard")
     (list "patient 10" "covid suspect")
     (list "patient 11" "covid no suspect")
     (list "patient 12" "covid contact")
     (list "patient 13" "covid no suspect")
     (list "patient 14" "covid suspect")
     (list "patient 15" "covid hard")
     (list "patient 16" "covid suspect")
    )
  )

  (define served-patients (serve-patients patients doctor-time patient-time))

  (display "Пацієнти, відправлені на самоізоляцію (вписаний лікарняний): ")
  (display (list-ref served-patients 0))
  (newline)
  (display "Пацієнти, відправлені на аналіз: ")
  (display (list-ref served-patients 1))
  (newline)
  (display "Пацієнти, відправлені до лікарні: ")
  (display (list-ref served-patients 2))
  (newline)
  (display "Пацієнти, що не були обслуговані: ")
  (display (list-ref served-patients 3))
  (newline)
)

(main)