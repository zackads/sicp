(load "proj4-util.scm")
(print ">>> Running tests for Common Exercise Part 2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 2
;;; Question 6: while
;;; 
;;; Checks the constructor and selector for while-
;;; blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define while-simple
"x = 3
while x < 5:
    x = x + 1

x")

(run-test "while-simple"
	  (run-python-string while-simple)
	  "5")
(define while-false
"x = 3
while False:
    x = x + 1

x")
(run-test "while-false"
	  (run-python-string while-false)
	  "3")

(define while-multi-line-body
"x = 3
y = 5
while x < 5:
    x = x + 1
    y = y + 1

x * y")

(run-test "while-multi-line-body"
	  (run-python-string while-multi-line-body)
	  "35")

(define while-else
"x = 3
while x < 5:
    x = x + 1
else:
    x = x * 10

x")
(run-test "while-else"
	  (run-python-string while-else)
	  "50")