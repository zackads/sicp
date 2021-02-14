#lang racket/base

(require berkeley)

(define (even? n) (equal? (remainder n 2) 0))
(define (odd? n) (not (even? n)))

(define (filter predicate? items)
  (cond ((empty? items) '())
        ((predicate? (car items)) (cons (car items) (filter predicate? (cdr items))))
        (else (filter predicate? (cdr items))) ))

(define (map function items)
  (if (null? items)
      nil
      (cons (fn (car items))
            (map fn (cdr items)))))