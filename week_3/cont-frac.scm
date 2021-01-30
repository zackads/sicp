#lang racket/base

(require berkeley)

(define (cont-frac n d k)
  (define (iter i n d k)
    (if (equal? i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (iter (+ i 1) n d k))) ))
  (iter 0 n d k))

(define (cont-frac2 n d k)
  (define (helper i)
    (if (equal? i k)
        0
        (/ (n i) (+ (d i) (helper (+ i 1))))))
  (helper 1))

(define (cont-frac-iter n d k)
  (define (iter i n d k result)
    (if (equal? i k)
        result
        (iter (+ i 1) n d k (/ result (d i)))))

  (iter 0 n d k (n 0)))

(define (cont-frac-iter2 n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter ( - k 1)
              (/ (n k) (+ (d k) result))) ))
  (iter k 0))

                