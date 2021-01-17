(define (add-streams s1 s2)(cond((stream-null? s1)s2)
				((stream-null? s2)s1)
				(else (cons-stream (+ (stream-car s1)
						(stream-car s2))
					     (add-streams
					      (stream-cdr s1)
					      (stream-cdr s2))))))

(define fibs (cons-stream 0
			  (cons-stream 1
				       (add-streams (stream-cdr fibs)
						    fibs))))
