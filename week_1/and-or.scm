#lang racket

(require berkeley)

; If `or` is an ordinary function, this will result in an infinite loop
; when the second condition is evaluated.
; If it is a special form, it'll just return true

(define (test-or)
  (or (< 9 10) (test-or)))

; If `and` is an ordinary function, this will evaluate in the applicative
; order and return true even when n is a random closure, e.g. (random 10).

(define (test-and n)
  (and (= 0 (- n n)) (= n (/ (+ n n) 2))))