#lang racket/base

(require berkeley)

(define (for-each function sequence)
  (cond ((empty? sequence) #t)
        (else (function (car sequence))
              (for-each function (cdr sequence)))))