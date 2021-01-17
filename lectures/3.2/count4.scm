(define make-count
  (let ((glob 0))
    (lambda ()
      (let ((loc 0))
        (lambda ()
	  (set! loc (+ loc 1))
	  (set! glob (+ glob 1))
          (list loc glob))))))
