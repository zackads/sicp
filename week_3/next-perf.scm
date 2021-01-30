#lang racket/base

(require berkeley)

(define (sum-of-factors n)
  (define (is-factor? a b) (equal? (remainder b a) 0))
  
  (define (accumulator i sum)
    (if (equal? i 0)
        sum
        (accumulator (- i 1) (+ sum (if (is-factor? i n)
                                   i
                                   0)))))

  (accumulator (quotient n 2) 0))

(define (next-perf n)
  (define (is-perf? n) (equal? (sum-of-factors n) n))
  
  (if (is-perf? (+ n 1))
      (+ n 1)
      (next-perf (+ n 1))))
        
                