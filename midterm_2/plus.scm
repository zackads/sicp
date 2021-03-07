#lang racket

(require berkeley)

(define attach-tag cons)
(define type-tag car)
(define contents cdr)

; Midterm 2, problem 4 (data-directed programming)

(put 'dyne 'cm 'erg)
(put 'ft 'in 12)

(put '+ 'ft 


(define (plus x y)
  (+ x y))

(plus (attach-tag 'ft 2) (attach-tag 'in 6))
(plus (attach-tag 'in 6) (attach-tag 'ft 2))

; Completely didn't understand this question.