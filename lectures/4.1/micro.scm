;;; Very small Scheme interpreter.  Leaves out one or two features.
;;; (The version in the book fills in the details.)

(define (scheme)
  (display "> ")
  (print (eval (read) the-global-environment))
  (scheme) )

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((symbol? exp) (lookup-in-env exp env))
	((special-form? exp) (do-special-form exp env))
	(else (apply (eval (car exp) env)
		     (map (lambda (e) (eval e env)) (cdr exp)) ))))

(define (apply proc args)
  (if (primitive? proc)
      (do-magic proc args)
      (eval (body proc)
	    (extend-environment (formals proc)
				args
				(proc-env proc) ))))
