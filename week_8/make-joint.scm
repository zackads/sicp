#lang racket

; (require berkeley)

(define (make-account balance secret-password)
  (define password secret-password)
  (define invalid-password-attempts 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) (lambda (x) "Nee-nah-nee-nah cops are on their way!"))
  (define (dispatch password m)
    (if (equal? password secret-password)
        (begin (set! invalid-password-attempts 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request: MAKE-ACCOUNT" m))))
        (begin (set! invalid-password-attempts (+ invalid-password-attempts 1))
               (if (> invalid-password-attempts 7)
                   (call-the-cops)
                   (lambda (x) "Incorrect password")))))
  dispatch)

(define (make-joint sub-account sub-pwd joint-pwd)
  (define (dispatch password m)
    (if (equal? password joint-pwd)
        (sub-account sub-pwd m)
        (lambda (x) "Incorrect joint password")))
  dispatch)


(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'invalid-password 'withdraw) 50) ;; Incorrect password
((paul-acc 'rosebud 'withdraw) 50) ;; 50