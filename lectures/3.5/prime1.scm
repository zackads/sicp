

(define (prime? n)
  (null? (filter (lambda (x) (= (remainder n x) 0))
                 (range 2 (- n 1)))))

(define (range from to)
  (if (> from to)
      '()
      (cons from (range (+ from 1) to))))


(define (filter predicate data)
  (cond ((null? data) '())
        ((predicate (car data))
         (cons (car data) (filter predicate (cdr data))))
        (else (filter predicate (cdr data)))))




;; try (filter prime? (range 2 100))
