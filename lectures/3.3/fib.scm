(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
	 (fib (- n 2)) )))

(define (fast-fib n)
  (if (< n 2)
      n
      (let ((old (get 'fib n)))
        (if (number? old)
            old
            (begin
             (put 'fib n (+ (fast-fib (- n 1))
                            (fast-fib (- n 2))))
             (get 'fib n))))))

