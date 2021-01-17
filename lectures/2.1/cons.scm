; implementation of pairs as functions

(define (cons x y)
  (lambda (which)
    (cond ((equal? which 'car) x)
	  ((equal? which 'cdr) y)
	  (else (error "Bad message to CONS" which)) )))


(define (car pair)
  (pair 'car))

(define (cdr pair)
  (pair 'cdr))



