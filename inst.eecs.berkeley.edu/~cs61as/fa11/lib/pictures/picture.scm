;; Solutions to Picture language project.
;; This project is almost entirely about believing in data abstraction;
;; except for the problems that ask you to write selectors for an
;; abstract data type, there are no CARs or CDRs here.

;; 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; 2.45

(define (split major minor)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (major painter (minor smaller smaller)))))
  splitter)

;; 2.46

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

; The order of the arguments was very important!
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

; or

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)       ; this one is different!

;; 2.48

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

;; 2.49

(define frame-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define X-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (segments->painter
   (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
         (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define wave
  (segments->painter
   (list (make-segment (make-vect 0.4 1) (make-vect 0.35 0.85))
         (make-segment (make-vect 0.35 0.85) (make-vect 0.4 0.65))
         (make-segment (make-vect 0.4 0.65) (make-vect 0.3 0.65))
         (make-segment (make-vect 0.3 0.65) (make-vect 0.15 0.6))
         (make-segment (make-vect 0.15 0.6) (make-vect 0 0.85))
         (make-segment (make-vect 0 0.65) (make-vect 0.15 0.4))
         (make-segment (make-vect 0.15 0.4) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.35 0.5))
         (make-segment (make-vect 0.35 0.5) (make-vect 0.25 0))
         (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0))
         (make-segment (make-vect 0.75 0) (make-vect 0.6 0.45))
         (make-segment (make-vect 0.6 0.45) (make-vect 1 0.15))
         (make-segment (make-vect 1 0.35) (make-vect 0.75 0.65))
         (make-segment (make-vect 0.75 0.65) (make-vect 0.6 0.65))
         (make-segment (make-vect 0.6 0.65) (make-vect 0.65 0.85))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.6 1)))))

;; 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Alternatively:
; 
; (define (rotate180 painter)
;   (flip-horiz (flip-vert painter)))
; 
; (define (rotate270 painter)
;   (flip-horiz (flip-vert (rotate90 painter))))


;; 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;; or

(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

; Don't just glaze over while reading that -- understand and enjoy it!
; Composition of functions makes this problem a lot easier.

;; 2.52 -- changes are in CAPITAL LETTERS below.

;; 2.52a

(define smiley
  (segments->painter
   (list (make-segment (make-vect 0.4 1) (make-vect 0.35 0.85))
         (make-segment (make-vect 0.35 0.85) (make-vect 0.4 0.65))
         (make-segment (make-vect 0.4 0.65) (make-vect 0.3 0.65))
         (make-segment (make-vect 0.3 0.65) (make-vect 0.15 0.6))
         (make-segment (make-vect 0.15 0.6) (make-vect 0 0.85))
         (make-segment (make-vect 0 0.65) (make-vect 0.15 0.4))
         (make-segment (make-vect 0.15 0.4) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.35 0.5))
         (make-segment (make-vect 0.35 0.5) (make-vect 0.25 0))
         (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0))
         (make-segment (make-vect 0.75 0) (make-vect 0.6 0.45))
         (make-segment (make-vect 0.6 0.45) (make-vect 1 0.15))
         (make-segment (make-vect 1 0.35) (make-vect 0.75 0.65))
         (make-segment (make-vect 0.75 0.65) (make-vect 0.6 0.65))
         (make-segment (make-vect 0.6 0.65) (make-vect 0.65 0.85))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.6 1))
         (MAKE-SEGMENT (MAKE-VECT 0.45 0.75) (MAKE-VECT 0.5 0.7))
         (MAKE-SEGMENT (MAKE-VECT 0.5 0.7) (MAKE-VECT 0.55 0.75)))))

;; 2.52b

(define (funny-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (funny-corner-split painter (- n 1))))
        (beside (below painter UP)
                (below RIGHT corner)))))

;; 2.52c

(define (face-out painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (FLIP-HORIZ PAINTER) n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;           WARNING: This file is outdated.
;   You're looking for picture.rkt, not picture.scm.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Code for CS61A project 2 -- picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

;; or

; (define flipped-pairs
;   (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) 200) 100)
	 (- (* (ycor-vect v1) 200) 100))
  (pendown)
  (setxy (- (* (xcor-vect v2) 200) 100)
	 (- (* (ycor-vect v2) 200) 100)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		    (make-vect 0.5 0.5)
		    (make-vect 1.0 0.5)
		    (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;;
;; Your code goes here
;;

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 2 0)
			       (make-vect 0 2)))
