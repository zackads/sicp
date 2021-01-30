#lang racket/base

(require berkeley)

(define (cont-frac n d k)
  (define (iter i n d k)
    (if (equal? i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (iter (+ i 1) n d k))) ))
  (iter 0 n d k))
