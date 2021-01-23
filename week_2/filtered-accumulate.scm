#lang racket

(require berkeley)

(define (1+ n) (+ n 1))

(define (filtered-accumulate combiner filter? null-value term a next b)
  (define (next-match a next filter?)
  (if (filter? (next a))
      (next a)
      (next-match (next a) next filter?)))
  (if (> a b)
      null-value
      (combiner (term a) (filtered-accumulate combiner filter? null-value term (next-match a next filter?) next b)) ))
  
(define (sum-squares-primes a b)
  (define (square n) (* n n))
  (filtered-accumulate + prime? 0 square a 1+ b))

(define (product-positive-coprimes n)
  (define (coprime? a) (= (gcd a n) 1))
  (filtered-accumulate * coprime? 1 (lambda (x) x) 1 1+ n) )

;; prime? predicate and helper functions borrowed from SICP 1.2.6

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; gcd borrowed from http://jaredkrinke.github.io/learn-scheme/1-2-5-greatestcommon.html

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))