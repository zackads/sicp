(load "proj4-util.scm")
(print ">>> Running tests for Common Exercise Part 1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 1
;;; Question 1: Ignore-Comment
;;; 
;;; Checks if comments are ignored for newline, block
;;; nested and eof
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ignore-comment-newline 
"x = 5 # I am a comment IGNORE ME
x")
(run-test "ignore-comment-newline" 
	  (run-python-string ignore-comment-newline)
	  "5")

(define ignore-comment-block
"def square(x): # This should be ignored
   return x*x

square(5)")

(run-test "ignore-comment-block"
	  (run-python-string ignore-comment-block)
	  "25")

(define ignore-comment-nest
"x = 3 # Om # nom nom
x")
(run-test "ignore-comment-nest"
	  (run-python-string ignore-comment-nest)
	  "3")

(define ignore-comment-eof
"5 # This line does not end with a newline")
(run-test "ignore-comment-eof"
	  (run-python-string ignore-comment-eof)
	  "5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 2
;;; Question 2: get-indent
;;; 
;;; Checks if indents are counted correctly
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-indent-no-indent
"x = 3")
(run-test "get-indent-no-indent"
	  (indentation (run-python-read get-indent-no-indent))
	  0)

(define get-indent-one-indent
" x = 3")
(run-test "get-indent-one-indent"
	  (indentation (run-python-read get-indent-one-indent))
	  1)

(define get-indent-three-indent
"   x = 3")
(run-test "get-indent-three-indent"
	  (indentation (run-python-read get-indent-three-indent))
	  3)


	  
