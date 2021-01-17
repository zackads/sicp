;; Step 2: primitive procedures

(define (input-loop)
  (display "=61A=> ")
  (flush)
  
  (let ((input (read)))
    (if (equal? input 'exit)
	(print "Au Revoir!")
	(begin
	  (print (mc-eval input))
          (input-loop)))))


(define (mc-eval exp)
  (cond ((number? exp) exp)
	((primitive? exp) (eval exp))
	((list? exp) (mc-apply (mc-eval (car exp))
			      (map mc-eval (cdr exp))))
	(else (error "UNKNOWN expression"))))


(define (mc-apply fn args)
  (do-magic fn args))


(define (do-magic fn args)
  (apply fn args))


(define (primitive? exp)
  (or (eq? exp '+)
      (eq? exp '-)
      (eq? exp '/)
      (eq? exp '*)
      (eq? exp 'car)
      (eq? exp 'cdr)
      (eq? exp 'cons)
      (eq? exp 'null?)
      ;; more
      ))

(input-loop)
