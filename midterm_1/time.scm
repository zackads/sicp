#lang racket/base

(require berkeley)

; Old constructor and selectors
; (define (make-time hr mn cat) (list hr mn cat))
; (define hour car)
; (define minute cadr)
; (define category caddr)

(define (make-time hr mn cat)
  (if (equal? cat 'am)
      (+ (* hr 100) mn)
      (+ 1200 (* hr 100) mn)))

(define (hour time) (remainder (/ (- time (minute time)) 100) 12))
(define (minute time) (remainder time 100))
(define (category time) (if (> (hour time) 11) 'pm 'am))

(define (24-hour time)
  (if (equal? (category time) 'pm)
      (+ 1200 (* (hour time) 100) (minute time))
      (+ (* (hour time) 100) (minute time))))

(define (time-print-form time)
  (define (format-minute minute)
    (if (< minute 10) (word '0 minute) minute))
  
  (word (hour time) ': (format-minute (minute time)) (category time)))

(time-print-form (make-time 3 07 'pm)) ; "3:07pm"
  
(24-hour (make-time 3 47 'pm)) ; 1547  
(24-hour (make-time 3 47 'am)) ; 347