#lang racket

(require berkeley)

(define (ordered-pair? a b)
  (< a b))
      
(define (ordered? numbers)
  (if (empty? numbers)
      false
      (ordered-pair? (first numbers) (first (butfirst numbers)))))