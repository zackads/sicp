(define (plurals sent)
  (if (empty? sent)
      '()
      (se (plural (first sent))
	  (plurals (butfirst sent)))))



(define (count sent)		;; wrong!
  (if (empty? sent)
      '()
      (+ 1 (count (butfirst sent)))))
