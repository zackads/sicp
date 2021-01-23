#lang racket

(require berkeley)

(define (product fn a b)
  (if (> a b)
      1
      (* (fn a) (product fn (+ a 1) b)) ))

(define (square n)
  (* n n)) 