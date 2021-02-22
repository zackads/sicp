;;; Sanity Check Fall 2000
;;; CS61A Project 2
;;; Erik Klavon / erik@eriq.org
;;; picture-test.scm
;;; This file contains all of the tests for project 2. Each test is a procedure
;;; which return true if all the tests in that procedure pass, false otherwise.
;;; The names of procedures to be run should be contained in a list named
;;; tests. 


; list of tests to be run
(define tests '(test-up-split test-split test-vectors test-frames test-frames-alt test-segments test-flip-horiz test-rotate180 test-rotate270 test-below test-below-alt))


; test-painter is a painter created to allow for the testing of this project.
; It does not draw anything, but generates a unique number for each frame it
; is given and adds that number to a global val.
(define (test-painter frame)
  (let ((frame-value (+ (expt 2 (ceiling (* 10 (xcor-vect (origin-frame frame)))))
			(expt 3 (ceiling (* 10 (ycor-vect (origin-frame frame)))))
			(expt 5 (ceiling (* 10 (xcor-vect (edge1-frame frame)))))
			(expt 7 (ceiling (* 10 (ycor-vect (edge1-frame frame)))))
			(expt 11 (ceiling (* 10 (xcor-vect (edge2-frame frame)))))
			(expt 13 (ceiling (* 10 (ycor-vect (edge2-frame frame))))))))
   
    (set! val (+ val frame-value))))


; test-frame is a reference frame used with test-painter
(define test-frame (make-frame (make-vect 0 0)
			       (make-vect 1 1)
			       (make-vect 0 1)))

; define val and make its initial value zero
(define val 0)

; test-up-split
(define (test-up-split)
  (set! val 0)
  ((up-split test-painter 3) test-frame)
  (= val 846627377.0))

; test-split
(define (test-split)
  (set! val 0)
  (((split beside beside) test-painter 4) test-frame)
  (= val 4273614135426.0))

; test-flip-horiz
(define (test-flip-horiz)
  (set! val 0)
  ((flip-horiz test-painter) test-frame)
  (= val 137858551923.0))

; test-rotate180
(define (test-rotate180)
  (set! val 0)
  ((rotate180 test-painter) test-frame)
  (= val 3486785426.0))

; test-rotate270
(define (test-rotate270)
  (set! val 0)
  ((rotate270 test-painter) test-frame)
  (= val 163795975501.0))

; test-below
(define (test-below)
  (set! val 0)
  ((below test-painter (beside test-painter test-painter)) test-frame)
  (= val 293453947.0))

; test-below-alt
(define (test-below-alt)
  (set! val 0)
  ((below-alt test-painter (beside test-painter test-painter)) test-frame)
  (= val 293453947.0))

; test-frames
(define (test-frames)
  (let ((a (make-vect 1 2))
	(b (make-vect 3 6)))
    (let ((c (add-vect a b))
	  (d (sub-vect a b)))
      (let ((e (make-frame a b c)))
	(and (= (xcor-vect (origin-frame e)) 1)
	   (= (ycor-vect (edge1-frame e)) 6)
	   (= (xcor-vect (edge2-frame e)) 4))))))

; test-frames-alt
(define (test-frames-alt)
  (let ((a (make-vect 1 2))
	(b (make-vect 3 6)))
    (let ((c (add-vect a b))
	  (d (sub-vect a b)))
      (let ((e (make-frame-alt a b c)))
	(and (= (xcor-vect (origin-frame-alt e)) 1)
	   (= (ycor-vect (edge1-frame-alt e)) 6)
	   (= (xcor-vect (edge2-frame-alt e)) 4))))))

; test-vectors
(define (test-vectors)
  (let ((a (make-vect 1 2))
	(b (make-vect 3 6)))
    (let ((c (add-vect a b))
	  (d (sub-vect a b))
	  (e (scale-vect 3 a)))
      (and (= (xcor-vect a) 1)
	   (= (ycor-vect b) 6)
	   (= (xcor-vect c) 4)
	   (= (ycor-vect d) -4)
	   (= (ycor-vect b) (ycor-vect e))))))

; test-segments
(define (test-segments)
  (let ((a (make-vect 1 2))
	(b (make-vect 3 6)))
    (let ((c (make-segment a b)))
      (and (= (xcor-vect (start-segment c)) 1)
	   (= (ycor-vect (end-segment c)) 6)))))

















