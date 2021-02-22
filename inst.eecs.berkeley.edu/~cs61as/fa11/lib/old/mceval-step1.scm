;; STEP 1: Read & Print

(define (input-loop)
  (display "=61A=> ")
  (flush)
  
  (let ((input (read)))
    (if (equal? input 'exit)
	(print "Au Revoir!")
	(begin
	  (print (mc-eval input))
          (input-loop)))))


(define (mc-eval exp)
  exp)


(input-loop)
