#lang racket/base

(require berkeley)

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1)))) ))

(define (iter-fast-expt b n)
  (define (iter invariant b n)
    (cond ((= n 0) invariant)
          ((even? n) (iter invariant (square b) (/ n 2)))
          (else (iter (* invariant b) b (- n 1))))
    )
  (iter 1 b n))