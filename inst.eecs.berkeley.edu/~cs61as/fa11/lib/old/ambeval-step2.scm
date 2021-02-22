
(define (input-loop next-try)
    (display "=61A=> ")
    (flush)
  
    (let ((input (read)))
      (cond ((equal? input 'exit)
	     (print "Au Revoir!"))
	    ((equal? input 'choose-another)
	     (next-try))
	    (else (begin
		  (print "Starting new problem:")
		  (mc-eval input
			   (lambda (val fail)
			     (print val)
			     (input-loop fail))
			   (lambda ()
			     (print "There are no more choices.")
			     (start-loop))))))))

(define (start-loop)
  (input-loop
   (lambda ()
      (print "There is no current problem.")
      (start-loop)))) 


(define (mc-eval exp success fail)
  (cond ((self-evaluating? exp) (success exp fail))
      ;;((self-evaluating? exp) exp)  ;; OLD
	((if-exp? exp)
	 (mc-eval (cadr exp)
		  (lambda (val fail2)
		    (if (not (eq? val 'nay))
			(mc-eval (caddr exp) success fail2)
			(mc-eval (cadddr exp) success fail2)))
		  fail))
	
        ((choose-exp? exp)
	    (eval-choose (cdr exp) success fail))

	 
	(else (error "UNKNOWN expression"))))


(define (self-evaluating? exp)
  (or (number? exp)
      (eq? exp 'aye)
      (eq? exp 'nay)))

(define (if-exp? exp)
  (eq? (car exp) 'if))

(define (choose-exp? exp)
  (and (list? exp)
       (eq? (car exp) 'choose)))


(define (eval-choose exps success fail)
  (if (null? exps)
      (fail)
      (mc-eval
       (car exps)
       (lambda (val fail2)
	 (success val fail2))
       (lambda ()
	 (eval-choose (cdr exps) success fail)))))


(start-loop)

