#|
READ THIS FIRST
This uses the picture language that you developed in project 2.
To make it work properly, you need to change the call to load below to
load your version of project 2.
There is no error checking in this version, so you can do some weird
stuff, like playing a piece in a position where there is already a
piece.  (In other words, you can cheat.)

If you want to use graphics, load the file and call play-game as shown
below.
If you want to use textual output, load the file, set text? to #t, and
then call play-game.

Sample interaction using textual output:
STk> (set! text? #t)  ; Don't use the graphics
okay
STk> (play-game (instantiate player "Rohin") (instantiate player "Dummy"))
Your turn, Rohin: (1 1)
#f #f #f 
#f x #f 
#f #f #f 
Your turn, Dummy: (1 0)
#f #f #f 
#f x #f 
#f o #f 
Your turn, Rohin: (0 0)
#f #f #f 
#f x #f 
x o #f 
Your turn, Dummy: (2 0)
#f #f #f 
#f x #f 
x o o 
Your turn, Rohin: (2 2)
#f #f x 
#f x #f 
x o o 
Rohin wins!
okay
|#

(load "~cs61as/lib/obj.scm")
(load "~/proj2/picture.scm")


(define (accum-or lst)
  (accumulate (lambda (x y) (or x y)) #f lst))
(define (accum-and lst)
  (accumulate (lambda (x y) (and x y)) #t lst))

(define-class (board)
  (instance-vars (grid (make-grid)))
  (method (piece x y) (get-piece grid x y))
  (method (play-move piece x y)
    (set! grid (next-grid grid piece x y)))
  (method (won? player-type)
    (or
     (accum-or ;; see below for a definition of accum-or and accum-and
      (map
       (lambda (x)
	 (accum-and
	  (map (lambda (y)
		 (equal? (ask self 'piece x y) player-type))
	       '(0 1 2))))
       '(0 1 2)))
     (accum-or
      (map
       (lambda (x)
	 (accum-and
	  (map (lambda (y)
		 (equal? (ask self 'piece y x) player-type))
	       '(0 1 2))))
       '(0 1 2)))
     (accum-and
      (map (lambda (x)
	     (equal? (ask self 'piece x x) player-type))
	   '(0 1 2)))
     (accum-and
      (map (lambda (x)
	     (equal? (ask self 'piece x (- 2 x)) player-type))
	   '(0 1 2))))))



(define-class (player name)
  (instance-vars (game #f) (player-piece #f))
  (method (start-game piece board)
    (set! game board)
    (set! player-piece piece))
  (method (make-move)
    (let ((move (read)))
      (ask game 'play-move player-piece (car move) (cadr move)))))

(define-class (tournament-player name trash-talk)
  (parent (player name))
  (class-vars (num-players 0))
  (initialize (set! num-players (+ num-players 1))
	      (display "Number of players: ") (display num-players) (newline))
  (method (make-move)
    (usual 'make-move)
    (display trash-talk) (newline)))

(define (play-game p1 p2)
  (let((brd (instantiate board)))
    (define (loop p)
      (display "Your turn, ") (display (ask p 'name)) (display ": ")
      (ask p 'make-move)
      (print-grid (ask brd 'grid))
      (if (ask brd 'won? (ask p 'player-piece))
	  (begin (display (ask p 'name)) (display " wins!") (newline))
	  (loop (if (equal? p p1) p2 p1))))
    (ask p1 'start-game 'x brd)
    (ask p2 'start-game 'o brd)
    (loop p1)))



; Graphics and the grid ADT (in other words, not OOP stuff)

(define (make-grid)
  (list
   (list #f #f #f)
   (list #f #f #f)
   (list #f #f #f)))

(define (get-piece grid x y)
  (list-ref (list-ref grid x) y))

(define (next-grid grid piece x y)
  (map (lambda (i)
	 (map (lambda (j)
		(if(and (= i x) (= j y))
		   piece
		   (get-piece grid i j)))
	      '(0 1 2)))
       '(0 1 2)))

(define text? #f)

(define (get-painter type)
  (if (equal? type 'x)
      xpainter
      opainter))

(define (print-grid grid)
  (if text?
      (for-each (lambda (y)
		  (for-each (lambda (x)
			      (display (get-piece grid x y)) (display " "))
			    '(0 1 2))
		  (newline))
		'(2 1 0))
      (begin
	(cs)
	(ht)
	(grid-painter full-frame)
	(for-each (lambda (y)
		    (for-each (lambda (x)
				(if (get-piece grid x y)
				    ((position-painter (get-painter (get-piece grid x y)) x y) full-frame)
				    'okay))
			      '(0 1 2)))
		  '(2 1 0)))))

(define grid-painter
  (segments->painter
   (list (make-segment (make-vect 0 (/ 1 3))
		       (make-vect 1 (/ 1 3)))
	 (make-segment (make-vect 0 (/ 2 3))
		       (make-vect 1 (/ 2 3)))
	 (make-segment (make-vect (/ 1 3) 0)
		       (make-vect (/ 1 3) 1))
	 (make-segment (make-vect (/ 2 3) 0)
		       (make-vect (/ 2 3) 1)))))

(define xpainter (segments->painter
		  (list (make-segment (make-vect 0 0)
				      (make-vect 1 1))
			(make-segment (make-vect 0 1)
				      (make-vect 1 0)))))

(define opainter (segments->painter
		  (list (make-segment (make-vect 0 0.5)
				      (make-vect 0.5 1))
			(make-segment (make-vect 0.5 1)
				      (make-vect 1 0.5))
			(make-segment (make-vect 1 0.5)
				      (make-vect 0.5 0))
			(make-segment (make-vect 0.5 0)
				      (make-vect 0 0.5)))))

(define (position-painter painter x y)
  (transform-painter painter
		     (make-vect (/ x 3) (/ y 3))
		     (make-vect (/ x 3) (/ (+ y 1) 3))
		     (make-vect (/ (+ x 1) 3) (/ y 3))))
