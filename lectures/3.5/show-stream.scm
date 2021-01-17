
(define (show-stream strm . args) ;2nd arg is how many
  (if (null? args)
      (ss1 strm 10 10)
      (ss1 strm (car args) (car args))))

(define ss show-stream)

(define (ss1 strm this all)
  (cond ((null? strm) '())
	((= this 0) '(...))
	((and (pair? strm) (procedure? (cdr strm)))
	 (cons (ss1 (stream-car strm) all all)
	       (ss1 (stream-cdr strm) (- this 1) all)))
	(else strm)))
