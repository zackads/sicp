;; Step 2.5: if and booleans

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
  (cond ((self-evaluating? exp) exp)
	((primitive? exp) (eval exp))
	((if-exp? exp)
	 (if (not (eq? (mc-eval (cadr exp)) 'nay))
	     (mc-eval (caddr exp))
	     (mc-eval (cadddr exp))))
	((list? exp) (mc-apply (mc-eval (car exp))
			      (map mc-eval (cdr exp))))
	(else (error "UNKNOWN expression"))))


(define (mc-apply fn args)
  (do-magic fn args))


;;;;;;;;;;;;;;;;;;;
;; Helper Procedures
;;;;;;;;;;;;;;;;;;

(define (if-exp? exp)
  (and (list? exp)
       (eq? (car exp) 'if)))


(define (boolean? exp)
  (or (eq? exp 'aye)
      (eq? exp 'nay)))


(define (do-magic fn args)
  (apply fn args))


(define (self-evaluating? exp)
  (or (number? exp)
      (eq? exp 'aye)
      (eq? exp 'nay)))

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
