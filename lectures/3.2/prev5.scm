(define make-previous
  (let ((glob 'first-time))
    (lambda ()
      (let ((old 'first-time))
	(lambda (msg)
	  (cond ((eq? msg 'local)
		 (lambda (arg)
		   (let ((result old))
		     (set! old arg)
		     result)))
		((eq? msg 'global)
		 (lambda (arg)
		   (let ((result glob))
		     (set! glob arg)
		     result)))
		(else (error "No such method" msg)) ))))))



