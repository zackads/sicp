#lang racket

(require berkeley)

(define (1+ n) (+ n 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate * 1 term (next a) next b)) ))

(define (product term a next b)
  (accumulate * 1 (lambda (x) x) a next b))

(define (factorial n)
  (accumulate * 1 (lambda (x) x) 1 1+ n))