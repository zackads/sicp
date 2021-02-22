(load "proj4-util.scm")
(print ">>> Running tests for Person A Part 2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person A Part 2
;;; Question 7a: for-loop
;;; 
;;; Tests the behavior of the for-loop (constructor,
;;; selector and eval)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define for-simple
"x = 1
for i in range(1,6):
    x = x*i

x")

(run-test "for-simple"
	  (run-python-string for-simple)
	  "120")

(define for-multi-line
"x = 1
y = 0
for i in range(1,6):
    x = x*i
    y = y+i

x+y")
(run-test "for-multi-line"
	  (run-python-string for-multi-line)
	  "135")

(define for-break
"x = 0
for i in range(1,6):
    x = i
    break

x")
(run-test "for-break"
	  (run-python-string for-break)
	  "1")

(define for-else
"x = 0
for i in range(1,6):
    x = i
else:
    x = x * 10

x")
(run-test "for-else"
	  (run-python-string for-else)
	  "50")
