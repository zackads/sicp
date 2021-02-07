#lang racket/base

(require berkeley)

(define (make-interval a b) (cons a b))

; Assuming a is always the lower-bound and b is always the upper-bound
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

; Assuming the arguments to make-interval are un-ordered
(define (upper-bound2 interval) (max  (car interval) (cdr interval)))
(define (lower-bound2 interval) (min (car interval) (cdr interval)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (spans-zero? interval) (< (lower-bound interval) 0))

(define (div-interval x y)
  (if (spans-zero? y)
      'error 
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent center percent)
  (define interval-width (* (* percent (/ center 100.0)) 2))
  (make-interval (- center interval-width)
                 (+ center interval-width)))

(define (percent interval)
  (* (/ (- (upper-bound interval) (center interval)) (center interval)) 100.0))



