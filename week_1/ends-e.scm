#lang racket

(require berkeley)

(define (ends-in-e? w)
  (equal? (last w) 'e))

(define (filter predicate w)
  (if
   (predicate w)
   w
   '()))

(define (ends-e s)
  (if (empty? s)
      '()
      (sentence
       (filter ends-in-e? (first s))
       (ends-e (butfirst s)))))