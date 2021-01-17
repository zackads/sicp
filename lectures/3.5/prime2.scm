

(define (stream-prime? n)
  (stream-null? (stream-filter (lambda (x) (= (remainder n x) 0))
			       (stream-range 2 (- n 1)))))

(define (stream-range from to)
  (if (> from to)
      the-empty-stream
      (cons-stream from (stream-range (+ from 1) to))))

(define (stream-filter predicate data)
  (cond ((stream-null? data) the-empty-stream)
        ((predicate (stream-car data))
         (cons-stream (stream-car data)
		      (stream-filter predicate (stream-cdr data))))
        (else (stream-filter predicate (stream-cdr data)))))

;; try (define s (stream-filter stream-prime? (stream-range 2 100)))
;; s
;; (display-stream s)

