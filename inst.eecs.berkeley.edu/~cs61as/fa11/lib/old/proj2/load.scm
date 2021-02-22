;;; Sanity Check Fall 2000
;;; CS61A Project 2
;;; Erik Klavon / erik@eriq.org
;;; load.scm
;;; This program checks to be sure that the student's code loads without
;;; producing an error.

;;; Definitions

; name of user's file to be checked
(define user-file "picture.scm")

; name and path of library file to be loaded
(define lib-file "~cs61a/lib/picture.scm")

; dummy defs so we can load picture.scm and users code
(define segments->painter (lambda (list) (lambda (frame) list)))

;;;; Code

; this loads the users code

(load user-file)

; we need to load library file just in case the user didn't

(load lib-file)

; this loads the users code again

(load user-file)

	
