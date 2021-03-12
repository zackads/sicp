#lang racket

(define f
  (let ((invocations 0))
    (lambda (x)
      (set! invocations (+ invocations 1))
      (cond ((= x 0) 0)
            ((even? invocations) 0)
            (else 1)))))
  
(+ (f 0) (f 1)) ;; 0
(+ (f 1) (f 0)) ;; 1

(define (make-account balance) 
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
           (error "Unknown request: MAKE-ACCOUNT"
                  m))))
  dispatch)