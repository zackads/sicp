#lang racket/base

(require berkeley)

(define (cont-frac n d k)
  (define (helper i)
    (if (equal? i k)
        0
        (/ (n i) (+ (d i) (helper (+ i 1))))))
  (helper 1))

(define (approx-e k)
  (define (n i) 1.0)

  (define (d i)
    (if (equal? (remainder i 3) 2)
        (* 2 (+ 1 (quotient (- i 2) 3)))
        1))

  (+ (cont-frac n d k) 2))
        
                