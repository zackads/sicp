#lang racket

(require berkeley)

(define (double procedure)
  (lambda (x) (procedure (procedure x))))
