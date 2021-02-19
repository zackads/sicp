#lang racket

; Exercise 2.45

(require sicp-pict)

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (split axis1 axis2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split axis1 axis2) painter (- n 1))))
          (axis1 painter (axis2 smaller smaller))))))

(paint ((split beside below) einstein 4))
(paint (right-split einstein 4))