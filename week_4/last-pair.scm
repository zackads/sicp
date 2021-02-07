#lang racket/base

(require berkeley)

(define (last-pair l)
  (if (equal? (cdr l) '())
      l
      (last-pair (cdr l))))