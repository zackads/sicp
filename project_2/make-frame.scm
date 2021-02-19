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

; First possible constructor with its selectors
; (define (make-frame origin edge1 edge2) (list origin edge1 edge2))
; (define (origin-frame f) (car f))
; (define (edge1-frame f) (cadr f))
; (define (edge2-frame f) (caddr f))

; Second possibly constructor with its selectors
(define (make-frame origin edge1 edge2) (cons origin (cons edge1 edge2)))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))

(make-frame (make-vect 0 0) (make-vect 2 4) (make-vect 6 8))
(origin-frame (make-frame (make-vect 0 0) (make-vect 2 4) (make-vect 6 8))) ; (0, 0)
(edge1-frame (make-frame (make-vect 0 0) (make-vect 2 4) (make-vect 6 8)))
(edge2-frame (make-frame (make-vect 0 0) (make-vect 2 4) (make-vect 6 8)))
