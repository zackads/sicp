(define (filter f strm)
  (cond ((empty-stream? strm) nil)
	((f (head strm)) (cons-stream (head strm) (filter f (tail strm))))
	(else (filter f (tail strm))) ))

(define (accumulate f start strm)
  (if (empty-stream? strm)
      start
      (f (head strm) (accumulate f start (tail strm))) ))

(define (range a b)
  (if (> a b)
      nil
      (cons-stream a (range (1+ a) b)) ))

(define (perfect? n)
  (= n (accumulate +
		   0
		   (filter (lambda (x) (= 0 (remainder n x)))
			   (range 1 (-1+ n)) ) )))
