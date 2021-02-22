;;; Sanity Check Fall 2000
;;; CS61A Project 2
;;; Erik Klavon / erik@eriq.org
;;; check.scm
;;; This program checks a list of procedure names to be tested, and exits 1 if
;;; one of them is not defined. It creates a file which contains the functions
;;; which are not bound.

;;; Definitions

; a list of procedures we want to make sure are defined by the user's program
(define user-procs '(up-split split make-vect xcor-vect ycor-vect add-vect sub-vect scale-vect make-frame origin-frame edge1-frame edge2-frame make-frame-alt origin-frame-alt edge1-frame-alt edge2-frame-alt make-segment start-segment end-segment frame-painter X-painter diamond wave flip-horiz rotate180 rotate270 below below-alt new-wave new-corner-split new-square-limit))

; total of file to output unbound function names to
(define output-file "not-defined")

; name of user's file to be checked
(define user-file "picture.scm")

; name and path of library file to be loaded
(define lib-file "~cs61a/lib/picture.scm")

; dummy defs so we can load user's code
(define segments->painter (lambda (list) (lambda (frame) list)))

;;;; Code

; check to make sure that a symbol is bound to a function
; if it isn't write the symbol to a line in the output file

(define (check-funct file)
  (lambda (sym)
    (let ((result (and (not (catch (eval sym))) (procedure? (eval sym)))))
      (if (not result)
	  (begin
	    (display sym file)
	    (newline file)))
      result)))
	    

; my-and needed to use and as a procedure

(define (my-and x y)
  (and x y))

; this loads the users code

(load user-file)

; load a library file just in case the user didn't copy it

(load lib-file)

; this loads the users code again

(load user-file)

; go through the list of specified function names, and create a list of true,
; false values. A side effect of this is that a message will be appened to a
; file if a function is not defined. Then, if there is an  undefined function,
; exit 1, else exit 0.

(call-with-output-file output-file
  (lambda (file)
    (if (not (accumulate my-and (map (check-funct file) user-procs)))
	(exit 1)
	(exit 0))))
