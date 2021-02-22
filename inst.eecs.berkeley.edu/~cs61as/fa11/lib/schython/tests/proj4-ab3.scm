(load "proj4-util.scm")
(print ">>> Running tests for Common Exercise Part 3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 3
;;; Question 8: dictionary
;;; 
;;; collects key-value
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dict-simple
  "{1 : 2}")

(run-test "dict-simple"
	  (run-python-string dict-simple)
	  "{
  1 : 2
}")

(define dict-two-kv
  "{1 : 2, 2 : 3}")
(run-test "dict-two-kv"
	  (run-python-string dict-two-kv)
	  "{
  1 : 2,
  2 : 3
}")

(define dict-empty
  "{}")
(run-test "dict-empty"
	  (run-python-string dict-empty)
	  "{
}")

(define dict-string
  "{'hello':'world'}")
(run-test "dict-string"
	  (run-python-string dict-string)
	  "{
  \"hello\" : \"world\"
}")

(define dict-error-colon
  "{1 : 2 : 3}")
(run-test "dict-error-colon"
	  (catch (run-python-string dict-error-colon))
	  #t)

(define dict-error-comma
  "{1 : 2 , 3 , 4}")
(run-test "dict-error-comma"
	  (catch (run-python-string dict-error-comma))
	  #t)

(define dict-in
  "1 in {1 : 2}")
(run-test "dict-in"
	  (run-python-string dict-in)
	  "True")

(define dict-not-in
  "0 not in {1 : 2}")
(run-test "dict-not-in"
	  (run-python-string dict-not-in)
	  "True")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 3
;;; Question 9: memoize
;;; 
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fib_memo (string-append 
		  (file->string "../memoize.py")
		  "
def __fib(x, memo):
    if x <= 1:
        return x
    else:
        return memo(x-1) + memo(x-2)

fib_memo = memoize(__fib)
"))

(run-test "fib-memo-base"
	  (run-python-string (string-append fib_memo
					    "fib_memo(1)"))
	  "1")

(run-test "fib-memo-small"
	  (run-python-string (string-append fib_memo
					    "fib_memo(10)"))
	  "55")

(newline)
(print ">>> WARNING: Note that for large values, the result of calling fib might be off because of overflow/rounding error")
(run-test "fib-memo-medium"
	  (run-python-string (string-append fib_memo
					    "fib_memo(50)"))
	  "12586269025")

(run-test "fib-memo-large"
	  (> (string->number (run-python-string (string-append fib_memo
							       "fib_memo(100)")))
	     300000000000000000000)
	  #t)
