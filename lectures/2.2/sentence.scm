(define (se a b)
  (cond ((word? a) (se (list a) b))
	((word? b) (se a (list b)))
	(else (append a b)) ))

(define (word? x)
  (or (symbol? x) (number? x)) )



