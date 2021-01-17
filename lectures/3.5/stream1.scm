(define (filter f lis)
  (cond ((null? lis) nil)
	((f (car lis)) (cons (car lis) (filter f (cdr lis))))
	(else (filter f (cdr lis))) ))

(define (accumulate f start lis)
  (if (null? lis)
      start
      (f (car lis) (accumulate f start (cdr lis))) ))

(define (range a b)
  (if (> a b)
      nil
      (cons a (range (1+ a) b)) ))

(define (perfect? n)
  (= n (accumulate +
		   0
		   (filter (lambda (x) (= 0 (remainder n x)))
			   (range 1 (-1+ n)) ) )))
