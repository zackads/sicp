;;;;;;;;; PICTURE-ASCII
;;; A different representation of painter that draws using ASCII art
;;; instead of the canvas widget. 
;;; Replace/comment out the definition of segments->painter in the original picture.scm 
;;; with this file or load this after picture.scm 
;;; STK> (load "picture-ascii.scm")
;;;
;;; To draw what is currently on the canvas, call the (draw-ascii) procedure
;;; To clear the canvas, call the (cs-ascii) procedure (cs stands for 'clear screen')
;;; STK> (draw-ascii)
;;; STK> (cs-ascii)
;;;
;;; Note that the picture "drawn" here would be slightly different
;;; We highly encourage you to test it with the actual canvas widget afterwards

;;; Applies F to every element on the vector
(define (vector-map f vect)
  (define new-vect (make-vector (vector-length vect)))
  (define (iter i)
    (if (= i (vector-length vect))
        new-vect
        (begin
          (vector-set! new-vect i (f (vector-ref vect i)))
          (iter (+ i 1)))
        )
    )
  (iter 0)
  )

(define MAXIMUM-CANVAS-SIZE 100)
(define CANVAS-SIZE (/ MAXIMUM-CANVAS-SIZE 2))
(define FILL "**") 
(define NOFILL "  ")
(define CANVAS (vector-map (lambda (ignore) (make-vector MAXIMUM-CANVAS-SIZE NOFILL ))
			   (make-vector MAXIMUM-CANVAS-SIZE)))

;;; Clears the canvas. Equivalent to Turtle Graphic's (cs)
(define (cs-ascii)
  (set! CANVAS (vector-map (lambda (ignore) (make-vector MAXIMUM-CANVAS-SIZE NOFILL ))
			   (make-vector MAXIMUM-CANVAS-SIZE))))

;;; Draws ascii art of the given canvas
(define (draw-ascii)
    (print-2d-vector CANVAS))

(define (segments->painter segment-list)
  (lambda (frame)  
    (for-each
     (lambda (segment)
       (draw-line-on-2dvector
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))
	CANVAS)
       )
     segment-list)
    )
  )


;;; Uses a simplified version of Bresenham's algorithm to 'draw' the line on the 2dvector
(define (draw-line-on-2dvector v1 v2 2dvector)

  ;;; Convets a number used in make-vect (unit square coordinate)
  ;;; into array coordinates
  (define (translate x)
    (min (- MAXIMUM-CANVAS-SIZE 1)
         (+  (/ CANVAS-SIZE 2)    ; Shifts it so that it is towards the middle instead of bottom left
             (* x CANVAS-SIZE) )) ; Scale it to the base canvas size
    )

  ;;; For each value of x between x1 and x2, find the corresponding y value
  (define (xiter x1 y1 x2 y2 deltax deltay)
    (if (< x1 x2)
        (let* ((x1 (max x1 0))
               (y1 (max y1 0))
               (roundx (round x1))
               (roundy (round y1)))
          (vector-set! (vector-ref 2dvector roundy) 
                       roundx
                       FILL)
          (xiter (+ x1 1) 
                 (+ y1 (/ deltay deltax)) 
                 x2 y2 deltax deltay)
          )
        )
    )

  ;;; For each value of y between y1 and y2, find the corresponding x value
  (define (yiter x1 y1 x2 y2 deltax deltay)
    (if (< y1 y2)
        (let* ((x1 (max x1 0))
               (y1 (max y1 0))
               (roundx (round x1))
               (roundy (round y1)))
          (vector-set! (vector-ref 2dvector roundy) 
                       roundx 
                       FILL)
          (yiter (+ x1 (/ deltax deltay))
                 (+ y1 1)
                 x2 y2 deltax deltay)
          )
        )
    )

  (let* ((x1 (translate (xcor-vect v1)))
         (y1 (translate (ycor-vect v1)))
         (x2 (translate (xcor-vect v2)))
         (y2 (translate (ycor-vect v2)))
         (deltax (- x2 x1))
         (deltay (- y2 y1))
         )

    ;;; Decide whether more points can be drawn by iterating through x or y
    (if (< (abs deltay) (abs deltax) )
        (if (< x1 x2)
            (xiter x1 y1 x2 y2 deltax deltay)
            (xiter x2 y2 x1 y1 deltax deltay))  
        (if (< y1 y2)
            (yiter x1 y1 x2 y2 deltax deltay)
            (yiter x2 y2 x1 y1 deltax deltay)
            )
        )
    )
  )


;;; Takes a 2D Vector and prints it row by row
(define (print-2D-vector 2dvect)
  ;;; prints the top most row (last row in the vector)
  (define (iter i)
    (if (>= i 0)
        (begin
          (vector-map (lambda (elem) (display elem)) 
                      (vector-ref 2dvect i))
          (display "\n") ; prints the next row
          (iter (- i 1)) )
        ))
  (iter (- (vector-length 2dvect) 1)))

;;;;; END OF PICTURE-ASCII
