(load "proj4-util.scm")
(print ">>> Running tests for Person A Part 1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person A Part 1
;;; Question 3a: Get-String
;;; 
;;; Checks if double/single quotes are used correctly
;;; to collect strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-string-single
"'This string uses single quote'")
(run-test "get-string-single"
	  (run-python-string get-string-single)
	  "This string uses single quote")

(define get-string-double 
"\"This string uses double quote\"")
(run-test "get-string-double"
	  (run-python-string get-string-double)
	  "This string uses double quote")

(define get-string-nest-single
"\"This string 'has nested single quote'\"")
(run-test "get-string-nest-single"
	  (run-python-string get-string-nest-single)
	  "This string 'has nested single quote'")

(define get-string-nest-double
"'This string \"has nested double quote\"'")
(run-test "get-string-nest-double"
	  (run-python-string get-string-nest-double)
	  "This string \"has nested double quote\"")

(define get-string-error-single
"'this is a string 'this is not' this is a string'")
(run-test "get-string-error-single"
	  (catch (run-python-string get-string-error-single))
	  #t)

(define get-string-error-double
"\"this is a string \"this is not\" this is a string\"")
(run-test "get-string-error-double"
	  (catch (run-python-string get-string-error-double))
	  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person A Part 1
;;; Question 4a: contains
;;; 
;;; Checks if a python list can check for "contains"
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks if contains work for ints
(define test-num-1 (make-py-num 3))
(define test-num-2 (make-py-num 2))
(define contains-int-list (make-py-list (list (make-py-num 3) (make-py-num 4)
					      (make-py-num 5) (make-py-num 6))))

(run-test "contains-int-true"
	  (ask (ask contains-int-list '__contains__ test-num-1) 'true?)
	  #t)

(run-test "contains-int-false"
	  (ask (ask contains-int-list '__contains__ test-num-2) 'true?)
	  #f)

;;; Checks if contains work for strings
(define test-string-1 (make-py-string "hello"))
(define test-string-2 (make-py-string "world"))
(define contains-string-list (make-py-list (list (make-py-string "hella") 
						 (make-py-string "world"))))

(run-test "contains-string-false"
	  (ask (ask contains-string-list '__contains__ test-string-1) 'true?)
	  #f)
(run-test "contains-string-true"
	  (ask (ask contains-string-list '__contains__ test-string-2) 'true?)
	  #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person A Part 1
;;; Question 5a: and/or
;;; 
;;; Special form "and' & "or" that short-circuits
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks for AND
(define and-true-simple
"True and True")
(run-test "and-true-simple"
	  (run-python-string and-true-simple)
	  "True")

(define and-false-simple
"True and False")
(run-test "and-false-simple"
	  (run-python-string and-false-simple)
	  "False")

(define and-true-int
"True and 3 and 5") ; Returns the last "true" thing
(run-test "and-true-int"
	  (run-python-string and-true-int)
	  "5")

(define and-false-int
"True and 3 and False")
(run-test "and-false-int"
	  (run-python-string and-false-int)
	  "False")

(define and-short-circuit
"False and (1 / 0)") ; Should short circuit and not error
(run-test "and-short-circuit"
	  (run-python-string and-short-circuit)
	  "False")

;;; Checks for OR
(define or-true-simple
"True or False")
(run-test "or-true-simple"
	  (run-python-string or-true-simple)
	  "True")

(define or-true-both
"True or True")
(run-test "or-true-both"
	  (run-python-string or-true-both)
	  "True")

(define or-false-simple
"False or False")
(run-test "or-false-simple"
	  (run-python-string or-false-simple)
	  "False")

(define or-true-int
"2 or 3 or 5")
(run-test "or-true-int"
	  (run-python-string or-true-int)
	  "2")

(define or-short-circuit
"2 or (1 / 0)")
(run-test "or-short-circuit"
	  (run-python-string or-short-circuit)
	  "2")