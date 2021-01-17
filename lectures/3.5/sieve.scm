(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x) (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes (sieve (stream-cdr integers)))
