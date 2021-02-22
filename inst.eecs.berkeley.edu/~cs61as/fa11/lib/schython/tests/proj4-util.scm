(load "../obj.scm")
(load "../parser.scm")
(load "../py-primitives.scm")
(load "../py-meta.scm")

#|
    Runs given test. All arguments are unevaluated
|#

(if (catch test-num) ;Prevents redefinition of test-num and test-passed
    (define test-num 0))

(if (catch test-passed)
    (define test-passed 0))
    
(define-macro (run-test func-name actual-result expected-result)
  `(let ((func-name ,func-name)
	 (result #f)
	 (expected-result ,expected-result))
     (set! test-num (+ 1 test-num))
     (format #t "Testing case ~A for ~A: " test-num func-name)
     (if (catch (set! result ,actual-result))
	 (format #t "Failed.~%Expected: ~A~%Got: ~A~%~%"
		 expected-result "ERROR")
	 (if (not (equal? result expected-result))
	     (format #t "Failed.~%Expected: ~A~%Got: ~A~%~%"
		     expected-result result)
	     (begin (set! test-passed (+ 1 test-passed))
		    (format #t "Passed!~%"))))))

       
#|
    Runs python code represented as a string using the Schython interpreter
    Suppreses error message and stdout
|#
(define (run-python-string python-string)
  (with-output-to-file "/dev/null" (lambda () (with-input-from-string python-string py-read-loop))))

(define (file->string file-path)
  (port->string (open-input-file file-path)))

(define (run-python-read python-string)
  (with-output-to-file "/dev/null" (lambda () (with-input-from-string python-string py-read))))

(define (py-read-loop)
  (set! the-global-environment (extend-environment '() '() '() ))
  (define-variable! '__name__ (make-py-string "__main__") the-global-environment)
  (define-primitives!)
  (define (loop return-value)
    (if (eof-object? (peek-char))
	(to-string return-value)
	(let ((line-obj (make-line-obj (py-read))))
	  (if (ask line-obj 'exit?)
	      'bye
	      (loop (eval-line line-obj the-global-environment))))))
  (loop 'done))
  

(define (to-string python-obj)
  (if (not (none? python-obj))
      (if (ask python-obj 'string?)
	  (ask python-obj 'val)
	  (ask (ask python-obj '__str__) 'val))
      *NONE*))

#|
    Summary of statistics of tests
|#
(define (print-test-results)
  (format #t "Passed ~A / ~A tests~%~%" test-passed test-num)
  (if (= test-passed test-num)
      (begin
	(print "****************************************************************************************************************************")
	(format #t
"
    
                                                   λλλλλ
                                              λλλλλλλλλλλλλλλ
                                           λλλλλλλλλ  λ λλλλλλλλ
                                          λλλλλλ            λλλλλλ
                                        λλλλλ                  λλλλ
                                       λλλλ      λλλλλλ         λλλλ
                                      λλλλ         λλλλλ         λλλλ
                                     λλλλ           λλλλ          λλλλ
                                     λλλλ           λλλλλ          λλλ
                                     λλλ           λλλλλλ          λλλ
                                    λλλλ          λλλλλλλλ         λλλλ
                                     λλλ         λλλλ λλλλ         λλλλ
                                     λλλ        λλλλλ  λλλλ        λλλ
                                     λλλλ      λλλλλ    λλλλ       λλλ
                                     λλλλ     λλλλ      λλλλλλλ   λλλλ
                                      λλλλ   λλλλλ       λλλλλλλ λλλλ
                                       λλλλ                     λλλλ
                                        λλλλλ                  λλλλ
                                         λλλλλλ             λλλλλλ
                                           λλλλλλλλλ    λλλλλλλλ
                                              λλλλλλλλλλλλλλλλ
                                                  λλλλλλλλ                
 _______  _______  _______  _______  _______  ______     _______  ___      ___        _______  _______  _______  _______  _______
|       ||   _   ||       ||       ||       ||      |   |   _   ||   |    |   |      |       ||       ||       ||       ||       |
|    _  ||  |_|  ||  _____||  _____||    ___||  _    |  |  |_|  ||   |    |   |      |_     _||    ___||  _____||_     _||  _____|
|   |_| ||       || |_____ | |_____ |   |___ | | |   |  |       ||   |    |   |        |   |  |   |___ | |_____   |   |  | |_____
|    ___||       ||_____  ||_____  ||    ___|| |_|   |  |       ||   |___ |   |___     |   |  |    ___||_____  |  |   |  |_____  |
|   |    |   _   | _____| | _____| ||   |___ |       |  |   _   ||       ||       |    |   |  |   |___  _____| |  |   |   _____| |
|___|    |__| |__||_______||_______||_______||______|   |__| |__||_______||_______|    |___|  |_______||_______|  |___|  |_______|
 ")
	(print "******************************************************************************************************************************"))))
	
