#lang racket

; Exercise 2.46

(require sicp-pict)

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(add-vect (make-vect 2 4) (make-vect 6 8)) ; (8, 12)
(sub-vect (make-vect 2 4) (make-vect 6 8)) ; (-4, -4)
(scale-vect 2 (make-vect 2 4)) ; (4, 8)