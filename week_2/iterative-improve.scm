#lang racket

(require berkeley)

(define (iterative-improve good-enough? improve)
  (define (improve-iter guess)
    (if (good-enough? guess)
        guess
        (improve-iter (improve guess)) ))
  improve-iter)
        

(define (average a b) (/ (+ a b) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? guess)
    (< (abs (- (f guess) guess))
       tolerance))
  (define (improve guess) (f guess))
  ((iterative-improve close-enough? improve) first-guess))