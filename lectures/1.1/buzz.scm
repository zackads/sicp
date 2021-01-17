(define (buzz n)
  (cond ((equal? (remainder n 7) 0) 'buzz)
	((member? 7 n) 'buzz)
	(else n)))
