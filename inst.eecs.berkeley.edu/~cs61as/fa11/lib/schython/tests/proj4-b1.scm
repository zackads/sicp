(load "proj4-util.scm")
(print ">>> Running tests for Person B Part 1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person B Part 1
;;; Question 3b: get-num
;;; 
;;; Collects numbers, including decimals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-num-int
"314")
(run-test "get-num-int"
	  (run-python-string get-num-int)
	  "314")

(define get-num-float
"3.14")
(run-test "get-num-float"
	  (run-python-string get-num-float)
	  "3.14")

;;; 3.foo should treat 3 as an int with a method call foo
;;; Not possible using the current construct
#|
(define get-num-int-str
"3.foo")
(run-test "get-num-int-str"
	  (tokens (run-python-read get-num-int-str))
	  '(3 .foo))
|#

(define get-num-float-str
"3.14.foo")
(run-test "get-num-float-str"
	  (tokens (run-python-read get-num-float-str))
	  '(3.14 .foo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person B Part 1
;;; Question 4b: negate-bool
;;; 
;;; Negates boolean
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-test "negate-bool-true"
	  (ask (negate-bool *PY-TRUE*) 'true?)
	   #f)

(run-test "negate-bool-false"
	  (ask (negate-bool *PY-FALSE*) 'true?)
	  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person B Part 1
;;; Question 5b: in/not in
;;; 
;;; Checks for membership in a list
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks for "in" infix-operator
(define in-int-true 
"1 in [1, 2, 3]")
(run-test "in-int-true"
	  (run-python-string in-int-true)
	  "True")

(define in-int-false
"4 in [1,2,3]")
(run-test "in-int-false"
	  (run-python-string in-int-false)
	  "False")

(define in-string-true
"'hello' in ['hello','world']")
(run-test "in-string-true"
	  (run-python-string in-string-true)
	  "True")

(define in-string-false
"'hella' in ['hello','world']")
(run-test "in-string-false"
	  (run-python-string in-string-false)
	  "False")

;;; Checks for "not in" infix-operator

(define not-in-int-true
"4 not in [1,2,3]")
(run-test "not-in-int-true"
	  (run-python-string not-in-int-true)
	  "True")

(define not-in-int-false
"1 not in [1, 2, 3]")
(run-test "not-in-int-false"
	  (run-python-string not-in-int-false)
	  "False")

(define not-in-string-true
"'hella' not in ['hello','world']")
(run-test "not-in-string-true"
	  (run-python-string not-in-string-true)
	  "True")

(define not-in-string-false
"'hello' not in ['hello','world']")
(run-test "not-in-string-false"
	  (run-python-string not-in-string-false)
	  "False")
