#lang racket

(require berkeley)

(define (square n)
  (* n n))

(define (squares numbers)
  (if (empty? numbers)
      '()
      (sentence (square (first numbers))
          (squares (butfirst numbers)))))