#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

; Graphical helpers courtesy of https://ericscrivner.me/2015/05/the-sicp-picture-language-in-racket/

(require sicp-pict)

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
             (end-coord-map ((frame-coord-map frame) (end-segment segment))))
         (line
          (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
          (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
     segment-list)))

(define unit-frame (make-frame (make-vect 0 500) (make-vect 500 0) (make-vect 0 -500)))

; Exercise 2.49

(define outline-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.1 0.1)
                  (make-vect 0.1 0.9))
    (make-segment (make-vect 0.1 0.9)
                  (make-vect 0.9 0.9))
    (make-segment (make-vect 0.9 0.9)
                  (make-vect 0.9 0.1))
    (make-segment (make-vect 0.9 0.1)
                  (make-vect 0.1 0.1)))))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 1 1))
    (make-segment (make-vect 0 1)
                  (make-vect 1 0)))))

(define diamond-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0.5)
                  (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1)
                  (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5)
                  (make-vect 0.5 0))
    (make-segment (make-vect 0.5 0)
                  (make-vect 0 0.5)))))

(outline-painter unit-frame)
(x-painter unit-frame)
(diamond-painter unit-frame)