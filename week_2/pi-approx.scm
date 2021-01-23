#lang racket

(require berkeley)

(define (product fn a b)
  (if (> a b)
      1
      (* (fn a)
         (product fn (+ a 1) b))))

(define (factorial n)
  (product (lambda (x) (* x 1)) 1 n))

(define (even? n)
  (and (= (modulo n 2) 0) (not (= n 1)) ))

(define (pi-approx-numerator n)
  (cond ((eq? n 1) (+ n 1))
        ((even? n) (+ n 2))
        (else (+ n 1)) ))

(define (pi-approx-denominator n)
  (cond ((not (even? n)) (+ n 2))
        (else (+ n 1)) ))

(define (pi-approx)
  (* 4.0 (/
        (product pi-approx-numerator 1 1000)
        (product pi-approx-denominator 1 1000)) ))