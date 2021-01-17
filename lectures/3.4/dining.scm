(define s (make-serializer))

(define free (list #t #t #t #t #t))

(define (philosopher num)
  (think)
  (try-to-eat num)
  (philosopher num))

(define (try-to-eat num)
  (if ((s (lambda ()
	    (if (and (list-ref free (left num))
		     (list-ref free (right num)))
		(begin
		 (set-car! (nth-cdr (left num) free) #f)
		 (set-car! (nth-cdr (right num) free) #f)
		 #t)
		#f))))
      (begin
       (eat)
       (set-car! (nth-cdr (left num) free) #t)
       (set-car! (nth-cdr (right num) free) #t))
      (try-to-eat num)))
