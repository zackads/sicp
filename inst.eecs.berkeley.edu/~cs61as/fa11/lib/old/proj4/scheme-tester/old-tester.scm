(load "logo.scm")
(load "logo-meta.scm")

(define (read-lines-from-string str)
  (with-input-from-string str
    (lambda ()
      (define (helper)
	(let ((line (read-string)))
	  (if (eof-object? line) '()
	      (cons (string-append line "\n")
		    (helper)))))
      (helper))))


(define recording #f)
(define transcript-record '())

(define old-logo-read logo-read)
(define old-prompt prompt)
(define old-display display)
(define old-print print)
(define old-newline newline)

(define record-input "")
(define (record-logo-read)
  (let ((line (string-append (read-string) "\n")))
    (set! record-input (string-append record-input line))
    (with-input-from-string line old-logo-read))) 
(define (record-prompt x)
  (old-display x)
  (flush))

(define record-output "")
(define (record-display thing)
  (set! record-output (string-append record-output
				     (with-output-to-string
				       (lambda ()
					 (old-display thing)))))
  (old-display thing))
(define (record-print thing)
  (record-display thing)
  (record-newline))
(define (record-newline)
  (set! record-output (string-append record-output "\n"))
  (old-newline))

(define (toggle-record bool)
  (if (equal? bool 'false)
      (begin 
	(record-off))
      (begin
	(record-on)) )
  '=no-value=)

(define (record-on)
  (if (not recording)
      (begin
	(display "Recording session.\n")
	(set! recording #t)
	(set! record-input "")
	(set! record-output "")
	(set! transcript-record '())
	(set! logo-read record-logo-read)
	(set! prompt record-prompt)
	(set! display record-display)
	(set! print record-print)
	(set! newline record-newline)
	))
  '=no-value=)

(define (record-off)
  (if recording
      (begin 
	(set! recording #f)
	(set! record-input "")
	(set! record-output "")
	(set! logo-read old-logo-read)
	(set! prompt old-prompt)
	(set! display old-display)
	(set! print old-print)
	(set! newline old-newline)
	(if (not (null? transcript-record))
	    (display "The recorded session is stored in the STk global variable transcript-record.\n"))
	))
  '=no-value=)

(add-prim 'record 1 toggle-record)
(add-prim 'bye 0 (lambda () '=exit=)) 

(define (one-eval)
  (let ((line (logo-read)))
    (if (not (null? line))
	(let ((result (eval-line (make-line-obj line)
				 the-global-environment)))
	  (if (eq? result '=exit=)
	      #f  ;; means quit
	      (begin 
		(if (not (eq? result '=no-value=))
		    (logo-print (list "You don't say what to do with" result)))
		#t)))
	#t)))

(define (driver-loop)
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
  (logo-read)
  (helper)
  (record-off)
  'okay
  )

(define (initialize-logo)
  (set! the-global-environment (extend-environment '() '() '()))
  (set! the-procedures the-primitive-procedures)
  (set! recording #f)
  (set! logo-read old-logo-read)
  (set! prompt old-prompt)
  (set! display old-display)
  (set! print old-print)
  (set! newline old-newline)
  (driver-loop))



(define tester-input '()) ;; list of strings
(define (tester-logo-read)
  (if (null? tester-input)
      (error "No more input in tester")
      (let ((line (car tester-input)))
	(old-display line)
	(set! tester-input (cdr tester-input))
	(with-input-from-string line old-logo-read))))
(define (tester-prompt x)
  (old-display x)
  (flush))

(define tester-output "")
(define (tester-display thing)
  (set! tester-output (string-append tester-output
				     (with-output-to-string
				       (lambda ()
					 (old-display thing)))))
  (old-display thing))
(define (tester-print thing)
  (tester-display thing)
  (tester-newline))
(define (tester-newline)
  (set! tester-output (string-append tester-output "\n"))
  (old-newline))


(define (tester-loop io-pairs)
  (define (test-one-line in out-ref)
    (set! tester-input (read-lines-from-string in))
    (one-eval)
    (let ((out tester-output))
      (if (not (string=? out-ref out))
	  (begin (old-display "############") (old-newline)
		 (old-display "Expected: ")
		 (old-display out-ref)
		 (old-newline)
		 (old-display "Got: ")		 
		 (old-display out)
		 (old-newline)
		 (old-display "############") (old-newline)
		 #f))
      (set! tester-output "")
      ))
  (define (helper io-pairs)
    (if (null? io-pairs) 'okay
	(begin
	  (prompt "? ")
	  (test-one-line (caar io-pairs) (cdar io-pairs))
	  (helper (cdr io-pairs)) )))
  
  (set! the-global-environment (extend-environment '() '() '()))
  (set! the-procedures the-primitive-procedures)
  
  (set! tester-input '())
  (set! tester-output "")
  (set! logo-read tester-logo-read)
  (set! prompt tester-prompt)
  (set! display tester-display)
  (set! print tester-print)
  (set! newline tester-newline)
  
  (helper io-pairs)

  (set! logo-read old-logo-read)
  (set! prompt old-prompt)
  (set! display old-display)
  (set! print old-print)
  (set! newline old-newline)

  'okay
  )
