(define music
  '((beatles (john lennon) (paul mccartney) (george harrison) (ringo starr))
    (who (pete townsend) (john alec entwistle) (roger daltrey) (keith moon))
    (zombies (rod argent) (chris white) (colin blunstone)
	     (paul atkinson) (hugh grundy))
    (- (elton john))
    (- (john sebastian))
    (dddbmt (david harmon) (trevor davies) (john dymond) (michael wilson)
	    (ian amey))))

(define groupname car)
(define members cdr)

(define firstname first)
(define lastname last)

(define (johns1 groups)
  (filter (lambda (x) (equal? (firstname x) 'john))
	  (flatten (map members groups))))

(define (flatten lst)
  (apply append lst))

(define (johns2 groups)
  (cond ((null? groups) '())
	((word? groups) '())
	((member? 'john groups) (list groups))
	(else (append (johns2 (car groups)) (johns2 (cdr groups))))))
