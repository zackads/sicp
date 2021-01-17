(define (make-serializer)
  (let ((in-use? #f))
    (lambda (proc)
      (define (protected-proc . args)
	(if in-use?
	    (begin
	     (wait-a-while)                 ; Never mind how to do that.
	     (apply protected-proc args))   ; Try again.
	    (begin
	     (set! in-use? #t)        ; Don't let anyone else in.
	     (apply proc args)        ; Call the original procedure.
	     (set! in-use? #f))))     ; Finished, let others in again.
      protected-proc)))
