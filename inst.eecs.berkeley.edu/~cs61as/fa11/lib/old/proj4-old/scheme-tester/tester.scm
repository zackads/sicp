

;; splits a string on \n, keeping them.
;; i.e.  a\nb\n\nc\n splits to
;;    a\n,  b\n,  \n,  c\n
(define (read-lines-from-string str)
  (with-input-from-string str
    (lambda ()
      (define (helper)
	(let ((line (read-string)))
	  (if (eof-object? line) '()
	      (cons (string-append line "\n")
		    (helper)))))
      (helper))))

(define *stdout* (current-output-port))

(define recording #f)
(define transcript-record '())

(define old-driver-loop driver-loop)
(define old-logo-read logo-read)
(define old-display display)
(define old-print print)
(define old-newline newline)

(define record-input "")
(define (record-logo-read)
  (let ((line (string-append (read-string) "\n")))
    (set! record-input (string-append record-input line))
    (set! record-output (string-append record-output line))
    (with-input-from-string line old-logo-read)))
(define record-output "")
(define (record-display thing)
  (if (eq? (current-output-port) *stdout*)
      (set! record-output (string-append record-output
					 (with-output-to-string
					   (lambda ()
					     (old-display thing))))))
  (old-display thing))
(define (record-print thing)
  (record-display thing)
  (record-newline))
(define (record-newline)
  (set! record-output (string-append record-output "\n"))
  (old-newline))

;; table for storing records
(define record-table '())
(define (add-record! name rec)
  (let ((p (assoc name record-table)))
    (if p (set-cdr! p rec)
	(set! record-table (append record-table (list (cons name rec)))))))
;	(set! record-table (cons (cons name rec) record-table)))))
(define (get-record name)
  (let ((p (assoc name record-table)))
    (if p (cdr p) #f)))
;; loading and saving tests from file
(define (save-tests file)
  (with-output-to-file file (lambda () (print record-table))))
(define (load-tests file)
  (set! record-table (with-input-from-file file (lambda () (read)))))

(define (record-on name)
  (if (not recording)
      (begin
	(display "Recording session ")
	(display name)
	(newline)
	(set! recording name)
	(set! record-input "")
	(set! record-output "")
	(set! transcript-record '())
	(set! driver-loop record-driver-loop)
	(set! logo-read record-logo-read)
	(set! display record-display)
	(set! print record-print)
	(set! newline record-newline)
	)) )

(define (record-off)
  (if recording
      (begin 
	(set! record-input "")
	(set! record-output "")
	(set! driver-loop old-driver-loop)
	(set! logo-read old-logo-read)
	(set! display old-display)
	(set! print old-print)
	(set! newline old-newline)
	(if (not (null? transcript-record))
	    (begin
	      (add-record! recording transcript-record)
	      (display "Session recorded under name ")
	      (display recording)
	      (newline)))
	(set! recording #f)
	)) )

;; returns whether to keep going or not
(define (one-eval)
  (define exit #f)
  (if (catch
       (let ((line (logo-read)))
	 (if (equal? line '(bye))
	     (set! exit #t)
	     (if (not (null? line))
		 (let ((result (eval-line (make-line-obj line)
					  the-global-environment)))
		   (if (not (eq? result '=no-value=))
		       (logo-print (list "You don't say what to do with" result)))  )) )) )
      (begin
 	(display "ERROR: ")
 	(display *last-error-message*)
 	(if (not (null? *last-error-arg*))
 	    (begin (display ": ")
 		   (display *last-error-arg*)))
 	(newline)
	))
  (not exit))

(define (record-driver-loop)
  (define (helper)
    (prompt "? ")
    (if (one-eval)
	(begin
	  (if (and recording
		   (not (string=? record-input ""))) 
	      (begin 
		(set! transcript-record
		      (append transcript-record
			      (list (cons record-input
					  record-output))))
		(set! record-input "")
		(set! record-output "")))
	  (helper))))
  (helper)
  )

(define (record-session name)
  (read-char) ;; get rid of initial newline
  (record-on name)
  (initialize-logo)
  (record-off))


(define tester-input '()) ;; list of strings
(define (tester-logo-read)
  (if (null? tester-input)
      (error "No more input in tester")
      (let ((line (car tester-input)))
	(old-display line)
	(set! tester-input (cdr tester-input))
	(set! tester-output (string-append tester-output line))
	(with-input-from-string line old-logo-read))))

(define tester-output "")
(define (tester-display thing)
  (if (eq? (current-output-port) *stdout*)
      (set! tester-output (string-append tester-output
					 (with-output-to-string
					   (lambda ()
					     (old-display thing))))))
  (old-display thing))
(define (tester-print thing)
  (tester-display thing)
  (tester-newline))
(define (tester-newline)
  (set! tester-output (string-append tester-output "\n"))
  (old-newline))


(define (tester-loop io-pairs re-recording)
  (define test-success #t)
  (define (test-one-line io-pair)
    (define line-success #t)
    (let ((in (car io-pair))
	  (out-ref (cdr io-pair)))
      (set! tester-input (read-lines-from-string in))    
      (one-eval)
      (let ((out tester-output))
	(if re-recording
	    (begin
	      (set-cdr! io-pair out))
	    (if (not (string=? out-ref out))
		(begin
		  (old-display "*** Expected:")
		  (old-newline)
		  (old-display out-ref)
		  (old-newline)
		  (old-display "*** Got:")
		  (old-newline)
		  (old-display out)
		  (old-newline)
		  (old-display "*** ")
		  (old-newline)
		  (set! line-success #f))))
	(set! tester-output "") )  )
    line-success)
  (define (helper io-pairs)
    (if (null? io-pairs) 'done
	(begin
	  (prompt "? ")
	  (if (not (test-one-line (car io-pairs)))
	      (set! test-success #f))
	  (helper (cdr io-pairs)) )))
    
  (set! tester-input '())
  (set! tester-output "")
  (set! logo-read tester-logo-read)
  (set! display tester-display)
  (set! print tester-print)
  (set! newline tester-newline)
  
  (set! driver-loop (lambda () (helper io-pairs)))
  (initialize-logo)
  (set! driver-loop old-driver-loop)

  (set! logo-read old-logo-read)
  (set! display old-display)
  (set! print old-print)
  (set! newline old-newline)

  test-success
  )


;; run test name
(define (run-test name)
  (tester-loop (get-record name) #f))

(define (run-all-tests)
  (define (helper tests num-passed)
    (if (null? tests)
	(begin (newline)
	       (display "*** Passed ")
	       (display num-passed)
	       (display " out of ")
	       (display (count record-table))
	       (display " tests.")
	       (newline))
	(begin
	  (newline)
	  (display "*** Running test ")
	  (display (caar tests))
	  (newline)
	  (helper (cdr tests) (+ num-passed (if (tester-loop (cdar tests) #f) 1 0))))))
  (helper record-table 0))
	      
(define (rat-file fn)
  (load-tests fn)
  (run-all-tests))


;; RERECORD a session

(define (re-record name)
  (tester-loop (get-record name) #t))

(define (re-record-all)
  (for-each re-record
	    (map car record-table)))
