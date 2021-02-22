;;; Sanity Check Fall 2000
;;; CS61A Project 2
;;; Erik Klavon / erik@eriq.org
;;; test.scm
;;; This file runs tests from a test file. The test file must contain all of
;;; the tests in procedures which return true if all the tests in that
;;; procedure pass, false otherwise. The names of procedures to be run should
;;; be contained in a list named tests. The names of tests which fail are
;;; output to the file output-file.

;;; Definitions

; name of file to output results to
(define output-file "tests-failed")

; the file which contains a list of tests to run called tests.
; Each test in this list should be defined as a procedure, and return true
; is the test is passed, false otherwise.
(define test-file "~cs61a/lib/proj2/picture-test.scm")

; name of user's file to be checked
(define user-file "picture.scm")

; name and path of library file to be loaded
(define lib-file "~cs61a/lib/picture.scm")      

; dummy def so we don't cause an error in loading the students code
(define (segments->painter foo) (lambda (bar) foo))

;;;; Code

; this loads the users code
(load user-file)

; load a library file just in case the user didn't copy it
(load lib-file)

; this loads the users code again
(load user-file)

; load our test file
(load test-file)

; check our tests procedure

(define (check-funct file)
  (lambda (sym)
    (let ((test (eval sym)))
      (if (not (test))
	  (begin
	    (display sym file)
	    (newline file)))

      (test))))
      	    
; my-and needed to use and as a procedure

(define (my-and x y)
  (and x y))

; go through the list of specified function names, and create a list of true,
; false values. A side effect of this is that a message will be appened to a
; file if a function is not defined. Then, if there is an  undefined function,
; exit 1, else exit 0.

(call-with-output-file output-file
  (lambda (file)
    (if (not (accumulate my-and (map (check-funct file) tests)))
	(exit 1)
	(exit 0))))



