(define (get key)
  (let ((record (assoc key (cdr the-table))))
    (if (not record)
	#f
	(cdr record))))

(define (put key value)
  (let ((record (assoc key (cdr the-table))))
    (if (not record)
	(set-cdr! the-table
		  (cons (cons key value)
			(cdr the-table)))
	(set-cdr! record value)))
  'ok)

(define the-table (list '*table*))
