;;; Vanilla nondeterministic evaluator with some interesting stuff traced
;;; Also fixed bug in eval-assignment
;;;
;;; Requires trace.scm and vambeval.scm
;;;
;;; When you see a procedure like AMB-SUCCEED in the trace, that
;;; refers to a (not necessarily unique) success continuation created
;;; by EVAL-AMB.
;;;
;;; After a while, the dots get a bit out of hand.  That's done
;;; intentionally, to demonstrate what's going on.  The reason ambeval
;;; doesn't run out of memory after a long time is that all of the
;;; procedures that call continuations make tail calls.  (The traced
;;; version actually would run out of memory, but that's another
;;; matter.)

(load "trace.scm")
(load "vambeval.scm")

(define (eval-application exp env succeed fail)
  (ambeval (operator exp)
	   env
	   (let ((application-success
		  (lambda (proc fail2)
		    (get-args (operands exp)
			      env
			      (let ((application-args-success
				    (lambda (args fail3)
				      (execute-application proc args succeed fail3))))
				(trace application-args-success mpp mpp)
				application-args-success)
			      fail2))))
	     (trace application-success mpp mpp)
	     application-success)
	   fail))

(define (eval-if exp env succeed fail)
  (ambeval (if-predicate exp)
	   env
	   (let ((if-succeed
		  (lambda (pred-value fail2)
		    (if (true? pred-value)
			(ambeval (if-consequent exp)
				 env
				 succeed
				 fail2)
			(ambeval (if-alternative exp)
				 env
				 succeed
				 fail2)))))
	     (trace if-succeed mpp mpp)
	     if-succeed)
	   fail))

(define (eval-definition exp env succeed fail)
  (ambeval (definition-value exp)
	   env
	   (let ((definition-success
		   (lambda (val fail2)
		     (define-variable! (definition-variable exp) val env)
		     (succeed 'ok fail2))))
	     (trace definition-success mpp mpp)
	     definition-success)
	   fail))

(define (eval-assignment exp env succeed fail)
  (ambeval (assignment-value exp)
	   env
	   (let ((assignment-success
		  (lambda (val fail2)
		    (let ((old-value
			   (lookup-variable-value (assignment-variable exp) env)))
		      (set-variable-value! (assignment-variable exp) val env)
		      (succeed 'ok
			       (let ((assignment-fail
				       (lambda ()
					 (set-variable-value! (assignment-variable exp) old-value env)
					 (fail2))))
				 (trace assignment-fail mpp mpp)
				 assignment-fail))))))
	     (trace assignment-success mpp mpp)
	     assignment-success)
	   fail))

(define (eval-amb exp env succeed fail)
  (define (try-next choices)
    (if (null? choices)
	(fail)
	(ambeval (car choices)
		 env
		 succeed
		 (let ((amb-fail
			(lambda ()
			  (try-next (cdr choices)))))
		   (trace amb-fail mpp mpp)
		   amb-fail))))
  (try-next (amb-choices exp)))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ") (newline)
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (let ((repl-success
			    (lambda (val next-alternative)
			      (announce-output output-prompt)
			      (user-print val)
			      (internal-loop next-alternative))))
		       (trace repl-success mpp mpp)
		       repl-success)
                     ;; ambeval failure
                     (let ((repl-fail
			    (lambda ()
			      (announce-output
			       ";;; There are no more values of")
			      (user-print input)
			      (driver-loop))))
		       (trace repl-fail mpp mpp)
		       repl-fail))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


;;; Trace some useful stuff

(trace ambeval mpp mpp)
(trace eval-application mpp mpp)
(trace get-args mpp mpp)
(trace execute-application mpp mpp)
(trace eval-if mpp mpp)
(trace eval-definition mpp mpp)
(trace eval-assignment mpp mpp)
(trace eval-amb mpp mpp)
(trace set-variable-value! mpp mpp)
(trace driver-loop mpp mpp)
