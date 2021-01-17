
(define (scheme)
  (display "> ")
  (print (eval (read)))
  (scheme))

(define (eval exp)
  (cond ((self-evaluating? exp) exp)
	((symbol? exp) (look-up-global-value exp))
	((special-form? exp) (do-special-form exp))
	(else (apply (eval (car exp))
		     (map eval (cdr exp))))))

(define (apply proc args)
  (if (primitive? proc)
      (do-magic proc args)
      (eval (substitute (body proc) (formals proc) args))))
