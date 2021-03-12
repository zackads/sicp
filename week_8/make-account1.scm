#lang racket

; (require berkeley)

(define (make-account balance secret-password)
  (define password secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (if (equal? password secret-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        (lambda (x) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ;; 60
((acc 'some-other-password 'deposit) 50) ;; "Incorrect password"
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50) ;; "Nee-nah-nee-nah cops are on their way!"