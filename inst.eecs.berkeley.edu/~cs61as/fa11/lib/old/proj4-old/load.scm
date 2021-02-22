;;; Sanity Check Fall 2000
;;; Project 4
;;; Erik Klavon / erik@eriq.org
;;; load.scm
;;; This program checks to be sure that the student's code loads without
;;; producing an error.

;;; Definitions

; None

;;;; Code
	
; load the provided files

; library files , berkeley.scm , etc
 (load "/home/aa/projects/scheme/lib/stk/site-scheme/load-simply")

;this loads the users code
(load "logo.scm")
(load "logo-meta.scm")

; exit

(exit)
	



