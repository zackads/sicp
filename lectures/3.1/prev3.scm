(define make-previous
  (lambda ()
    (let ((old 'first-time))
      (lambda (arg)
	(let ((result old))
	  (set! old arg)
	  result)))))



