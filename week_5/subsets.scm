#lang racket/base

(require berkeley)

(define *append append)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (*append rest (map (lambda (set) (cons (car s) set)) rest)))))

(set! subsets subsets)
(set! *append *append)
(trace subsets *append)

(subsets (list 1 2 3))