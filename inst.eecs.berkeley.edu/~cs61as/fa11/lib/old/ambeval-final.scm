;; Metacircular Evaluator: final version

(define (input-loop next-try)
  (display "=61A(Non-deterministic)=> ")
  (flush)
  
  (let ((input (read)))
    (cond ((equal? input 'exit)
	   (print "Au Revoir!"))
	  ((equal? input 'choose-another)
	   (next-try))
	  (else (begin
		  (print "Starting new problem:")
		  (mc-eval input the-global-env
			   (lambda (val fail)
			     (print val)
			     (input-loop fail))
			   (lambda ()
			     (print "There are no more choices")
			     (start-loop))))))))

(define (start-loop)
  (mc-eval '(define (require cond)
	      (if (not cond) (choose) 'okay)) the-global-env
	      (lambda (val fail) 'blah)
	      (lambda () 'blah))
  
  (input-loop (lambda ()
		(print "There is no current problem")
		(start-loop))))

(define (mc-eval exp env success fail)
  (cond ((self-evaluating? exp)
	   (success exp fail))
	
	((variable? exp)
	  (success (lookup-variable-value exp env) fail))

	((or-exp? exp) (eval-or (cdr exp) env success fail))

	((and-exp? exp) (eval-and (cdr exp) env success fail))
	
	((if-exp? exp)
	 (mc-eval (cadr exp) env
		  (lambda (pred fail2)
		    (if (not (eq? pred 'nay))
			(mc-eval (caddr exp) env success fail2)
			(mc-eval (cadddr exp) env success fail2)))
		  fail))

	((let-exp? exp)
	 (mc-eval (let->lambda exp) env success fail))
	
	((begin-exp? exp)
	 (eval-sequence (cdr exp) env success fail))
	
	((quote-exp? exp)
	 (success (cadr exp) fail))
	
	((set-exp? exp)
	   (mc-eval (caddr exp) env
		    (lambda (val fail2)
		      (let ((old-val (lookup-variable-value (cadr exp) env)))
			(set-variable-value! (cadr exp) val env)
			(success 'okay
			       (lambda ()
				 (set-variable-value! (cadr exp)
						      old-val env)
				 (fail2)))))
		      fail))

	((definition? exp)
	 (if (list? (cadr exp))
	     (mc-eval (define->lambda exp) env success fail)
	     (mc-eval (caddr exp) env
		      (lambda (val fail2)
			(define-variable! (cadr exp) val env)
			(success 'okay fail2))
		      fail)))
	     
	((lambda-exp? exp)
	 (success (make-procedure (cadr exp) (cddr exp) env) fail))

	((choose-exp? exp) (eval-choose (cdr exp) env success fail))
	 
	((list? exp)
	 (mc-eval (car exp) env
		  (lambda (fn fail2)
		    (eval-args (cdr exp) env
			       (lambda (arg-vals fail3)
				 (mc-apply fn arg-vals success fail3))
			       fail2))
		  fail))
			       
	(else (error "UNKNOWN expression"))))


(define (mc-apply fn args success fail)
  (cond ((lambda-proc? fn)
	  (eval-sequence (body fn)
			 (extend-environment
			     (params fn)
			     args
			     (env fn)) success fail))
       (else (success (do-magic fn args) fail))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Procedure ADT ;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (params p)
  (cadr p))

(define (body p)
  (caddr p))

(define (env p)
  (cadddr p))

(define (lambda-proc? p)
  (and (list? p)
       (eq? (car p) 'procedure)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-exp? exp)
  (eq? (car exp) 'and))

(define (eval-and exps env success fail)
  (if (null? (cdr exps))
      (mc-eval (car exps) env
	       success fail)
      (mc-eval (car exps) env
	       (lambda (first-val fail2)
		 (if (eq? first-val 'nay)
		     (success 'nay fail2)
		     (eval-and (cdr exps) env success fail2)))
	       fail)))

(define (or-exp? exp)
  (eq? (car exp) 'or))

(define (eval-or exps env success fail)
  (if (null? exps)
      (success 'nay fail)
      (mc-eval (car exps) env
	       (lambda (first-val fail2)
		 (if (not (eq? first-val 'nay))
		     (success first-val fail2)
		     (eval-or (cdr exps) env success fail2)))
	       fail)))

(define (let-exp? exp)
  (eq? (car exp) 'let))

(define (let->lambda exp)
  (let ((bindings (cadr exp)))
    (cons (make-lambda (map car bindings)
                                   (cddr exp))
                      (map cadr bindings))))
                     

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
  

(define (eval-args exps env success fail)
  (if (null? exps)
      (success '() fail)
      (mc-eval (car exps) env
	       (lambda (first-val fail2)
		 (eval-args (cdr exps)
			    env
			    (lambda (rest-vals fail3)
			      (success (cons first-val rest-vals) fail3))
			    fail2))
	       fail)))

(define (choose-exp? exp)
  (eq? (car exp) 'choose))

(define (eval-choose exps env success fail)
  (if (null? exps)
      (fail)
      (mc-eval (car exps) env
	       success
	       (lambda ()
		 (eval-choose (cdr exps) env success fail)))))


(define (quote-exp? exp)
  (eq? (car exp) 'quote))


(define (define->lambda exp)
  (list 'define (caadr exp)
	(append (list 'lambda (cdadr exp)) (cddr exp))))


(define (set-exp? exp)
  (eq? (car exp) 'set!))


(define (lambda-exp? exp)
  (eq? (car exp) 'lambda))


(define (begin-exp? exp)
  (eq? (car exp) 'begin))


(define (eval-sequence exps env success fail)
  (cond ((null? (cdr exps)) (mc-eval (car exps) env success fail))
	(else
	  (mc-eval (car exps) env
	        (lambda (first-val fail2)
		    (eval-sequence (cdr exps) env success fail2))
		fail))))
  

(define (if-exp? exp)
  (and (list? exp)
       (eq? (car exp) 'if)))


(define (boolean? exp)
  (or (eq? exp 'aye)
      (eq? exp 'nay)))


(define (do-magic fn args)
  (apply fn args))


(define (definition? exp)
  (eq? (car exp) 'define))

(define (variable? exp)
  (symbol? exp))

(define (self-evaluating? exp)
  (or (number? exp)
      (boolean? exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Primitives ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (yell wd)
  (word wd '!!))

(define (square num)
  (* num num))

(define (factorial num)
  (if (= num 0) 1
      (* num (factorial (- num 1)))))

(define (new-null? ls)
  (if (null? ls)
      'aye
      'nay))

(define (new-= num1 num2)
  (if (= num1 num2)
      'aye
      'nay))

(define (new-< num1 num2)
  (if (< num1 num2)
      'aye
      'nay))

(define (new-> num1 num2)
  (if (> num1 num2)
      'aye
      'nay))

(define (new-not cond)
  (if (eq? cond 'nay)
      'aye
      'nay))

(define (all-distinct ls)
  (if (null? ls) #t
      (and (not (member (car ls) (cdr ls)))
	   (all-distinct (cdr ls)))))

(define (new-all-distinct ls)
  (if (all-distinct ls) 'aye 'nay))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Related ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extend-environment vars vals base-env)
      (cons (cons vars vals) base-env))

(define (define-variable! var val env)
  (define first-frame (car env))
  (define (scan vars vals)
    (cond ((null? vars)
	   (set-car! first-frame (cons var (car first-frame)))
	   (set-cdr! first-frame (cons val (cdr first-frame))))
	  ((eq? var (car vars))
	   (set-car! vals val))
	  (else
	   (scan (cdr vars) (cdr vals)))))
  (scan (car first-frame) (cdr first-frame))
  var)

(define the-global-frame
  (cons (list '+ '- '/ '* 'car 'cdr 'cons 'null? 'nil 'yell 'square 'factorial
	       '=   '<    'list 'length '> 'not 'random 'all-distinct)
	(list + - / * car cdr cons new-null? nil yell square factorial
	      new-= new-<  list  length new-> new-not random new-all-distinct)))


(define the-global-env
  (cons the-global-frame nil))


(define (set-variable-value! var val env)
  (define first-frame (car env))
  (define (scan vars vals)
    (cond ((null? vars)
	      (if (eq? env the-global-env)
		  (error "Unbound Variable")
		  (set-variable-value! var val (cdr env))))
	  ((eq? var (car vars)) (set-car! vals val))
	  (else (scan (cdr vars) (cdr vals)))))
  (scan (car first-frame)
	(cdr first-frame)))


(define (lookup-variable-value var env)
  (define first-frame (car env))
  (define (scan vars vals)
    (cond ((null? vars)
	      (if (eq? env the-global-env)
		  (error "Unbound Variable")
		  (lookup-variable-value var (cdr env))))
	  ((eq? var (car vars)) (car vals))
	  (else (scan (cdr vars) (cdr vals)))))
  (scan (car first-frame)
	(cdr first-frame)))

(start-loop)
