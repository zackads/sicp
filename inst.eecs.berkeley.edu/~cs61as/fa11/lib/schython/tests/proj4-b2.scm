(load "proj4-util.scm")
(print ">>> Running tests for Person B Part 2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person B Part 2
;;; Question 7B: if-elif-else
;;; 
;;; Tests the behavior of the if-blocks  (constructor,
;;; selector and eval)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tests JUST the if
(define if-true
"x = 1
if x == 1:
    x = 2

x")

(run-test "if-true"
	  (run-python-string if-true)
	  "2")

(define if-false
"x = 1
if x == 2:
    x = 2

x")

(run-test "if-false"
	  (run-python-string if-false)
	  "1")

;;; Tests if-else
(define if-else-true
"x = 1
if x == 1:
    x = 2
else:
    x = 3

x")
(run-test "if-else-true"
	  (run-python-string if-else-true)
	  "2")


(define if-else-true
"x = 2
if x == 1:
    x = 2
else:
    x = 3

x")
(run-test "if-else-true"
	  (run-python-string if-else-true)
	  "3")


;;; Tests if-elif-else
(define if-elif-else-true
"x = 1
if x == 3:
    x = 2
elif x < 3:
    x = 3
else:
    x = 4

x")
(run-test "if-elif-else-true"
	  (run-python-string if-elif-else-true)
	  "3")

(define if-elif-else-false
"x = 5
if x == 3:
    x = 2
elif x < 3:
    x = 3
else:
    x = 4

x")
(run-test "if-elif-else-false"
	  (run-python-string if-elif-else-false)
	  "4")

(define if-elif-else-multiple
"x = 4
if x == 3:
    x = 2
    x = x * 2
elif x < 3:
    x = 3
    x = x * 2
elif x < 5:
    x = 4
    x = x * 2
else:
    x = 5
    x = x * 2

x")
(run-test "if-elif-else-multiple"
	  (run-python-string if-elif-else-multiple)
	  "8")