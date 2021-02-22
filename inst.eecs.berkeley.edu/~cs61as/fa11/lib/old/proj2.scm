(define LINE-START-X -200)
(define LINE-START-Y 150)
(define CHAR-HEIGHT 7)
(define SPACE-BTWN-CHARS 2)
(define DEFAULT-SIZE 12)

(define (init-display)
  (clearscreen)
  (move-to LINE-START-X LINE-START-Y) )

; You decide how to represent headings.
(define UP _____________ )

; Return a character, a procedure that, given a scale and a point to start
; drawing, draws the scaled character starting at the (scaled) point and 
; returns the (scaled) width of the character.  The scaled x distance of 
; the point to start drawing is included in the width.
; Cmd-list is a list of turtle graphics commands.
; Only the following turtle graphics commands are legal in this list:
;    forward, penup, pendown, right, left.
; Moreover, the argument to "right" or "left" must be 90, and the argument
; to "forward" must be greater than 0.

(define (make-character cmd-list)
  (lambda (scale point)
    (let
      ((current-x (xcor))
       (current-y (ycor)))
      ; move to point, then start drawing.
      (move-to 
        (+ current-x (* scale (x-coord point)))
        (+ current-y (* scale (y-coord point))) )
      (execute-each cmd-list scale)
      ; draw the character, then go back where we started.
      (move-to current-x current-y)
      ; return the furthest we got in the x direction.
      (find-max-x cmd-list scale (* scale (x-coord point)) UP) ) ) )

; Return the maximum distance travelled in the x direction
; with the given commands using the given scale and starting at
; the given x coordinate and heading.
; You have to supply this procedure.
(define (find-max-x cmd-list scale x heading)
   _______________________ )

; Execute the commands in the given list.
(define (execute-each cmd-list scale)
  (cond
    ((null? cmd-list) #t)
    (else 
     (execute-cmd (car cmd-list) scale)
     (execute-each (cdr cmd-list) scale) ) ) )

; Execute a turtle command.
; You have to complete this procedure; don't change any of the
; existing code.
(define (execute-cmd cmd scale)
  ((cadr 
     (assq
       (car cmd) 
       (list _______________________ ) ) )) )

; Draw a line of characters.
(define (draw-line chars)
  (hideturtle)
  (draw-line-helper chars 0 DEFAULT-SIZE)
  (carriage-return DEFAULT-SIZE)
  (showturtle) )

(define (draw-line-helper chars base-line scale)
  (cond
    ((null? chars) #t)
    ((character? (car chars))
     (move-to 
       (+ 
         (xcor) 
         ((car chars) scale (make-point 0 base-line)) 
         (* SPACE-BTWN-CHARS scale)) 
       (ycor))
     (draw-line-helper (cdr chars) base-line scale) ) 
    (else "*** bad character in line") ) )

(define character? procedure?)

; Go to the start of the next line.
(define (carriage-return scale)
  (move-to LINE-START-X (- (ycor) (* CHAR-HEIGHT scale))) )

;************************************************************************
; Low-level turtle movement procedure
; Moves turtle to a given position, leaving it facing up with the pen up.

(define (move-to x y)
  (penup)
  (setxy x y) 
  (setheading 0) )

;************************************************************************
; "point" abstract data type constructor + selectors

(define make-point list)
(define x-coord car)
(define y-coord cadr)

;************************************************************************
; test character

(define plus-sign-list 
  '((penup) (forward 3) (right 90)
            (pendown) (forward 4)
            (right 90) (right 90) (forward 2)
            (left 90) (forward 2)
            (right 90) (right 90) (forward 4)) )

(define plus-sign (make-character plus-sign-list))
