#lang racket/base

(require berkeley)

(define (even? n) (equal? (remainder n 2) 0))
(define (odd? n) (not (even? n)))

(define (filter predicate? sequence)
  (cond ((empty? sequence) '())
        ((predicate? (car sequence)) (cons (car sequence) (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence))) ))

(define (same-parity n . w)
  (cons n (if (even? n) (filter even? w) (filter odd? w))))