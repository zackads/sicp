
(define (loop n)
  (newline)
  (print "=New Trial=:")
  
  (define saving-acct 1000)
  (define checking-acct 100)
  
  (define (check-withdraw x)            ;; WITHDRAW FROM CHECKING
    (if (<= x checking-acct)
	(begin   (set! checking-acct (- checking-acct x))   x)
	"Don't have enough"))

  (define (save-deposit x)              ;; DEPOSIT TO SAVING
    (set! saving-acct (+ x saving-acct)))
  

  (define (transfer x)                 ;; TRANSFER FROM SAVING TO CHECKING
    (if (<= x saving-acct)
	(begin (set! saving-acct (- saving-acct x))
	       (set! checking-acct (+ checking-acct x)))
	"Don't have enough"))

  
  (define S (make-serializer))
  (define T (make-serializer))
  
  (define serial-withdraw (S check-withdraw))  
  (define serial-deposit (T save-deposit))
  (define serial-transfer (T (S transfer))) 
  
  (parallel-execute (lambda () (print (serial-withdraw 100)))
		    (lambda () (print (serial-withdraw 100)))
		    (lambda () (serial-deposit 200))
		    (lambda () (serial-transfer 10))

  (display "Checking: ") (print checking-acct)
  (display "Saving:" ) (print saving-acct)

  (if (not (= n 0))
      (loop (- n 1))))
