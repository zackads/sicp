#lang racket/base

(require berkeley)

(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
      (fold-left op
                 (op (car sequence) initial)
                 (cdr sequence))))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

(set! fold-left fold-left)
(trace fold-left)

(set! fold-right fold-right)
(trace fold-right)

(fold-right - 1 (list 1 2 3 4 5 6))
(fold-left - 1 (list 1 2 3 4 5 6))
        