(define make-previous
  (let ((glob 'first-time))
    (lambda ()
      (let ((old 'first-time))
	(lambda (arg)
	  (let ((result (list old glob)))
	    (set! old arg)
	    (set! glob arg)
	    result))))))



