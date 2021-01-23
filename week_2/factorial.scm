#lang racket

(require berkeley)

(define (product fn a b)
  (if (> a b)
      1
      (* (fn a) (product fn (+ a 1) b)) ))

(define (factorial n)
  (product (lambda (x) (* x 1)) 1 n))