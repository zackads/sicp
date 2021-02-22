;; Analyzing Evaluator

(define (input-loop)
  (display "=61A(Analyzer)=> ")
  (flush)
  
  (let ((input (read)))
    (if (equal? input 'exit)
	(print "Au Revoir!")
	(begin
	  (print (mc-eval (analyze input) the-global-env))
          (input-loop)))))

(define (analyze exp)
  (cond ((self-evaluating? exp) (lambda (env) exp))
	((variable? exp) (lambda (env) (lookup-variable-value exp env)))

	((quote-exp? exp)
	 (lambda (env) (cadr exp)))
	
	((if-exp? exp)
	 (lambda (env)
	   (let ((analyzed-tf (analyze (cadr exp)))
	         (analyzed-true (analyze (caddr exp)))
	         (analyzed-false (analyze (cadddr exp))))
	     (if (not (eq? (mc-eval analyzed-tf env) 'nay))
		 (mc-eval analyzed-true env)
		 (mc-eval analyzed-false env)))))
	 
	((definition? exp)
	 (if (list? (cadr exp))
	     (analyze (define->lambda exp))
	     (lambda (env)
	       (let ((analyzed-value (analyze (caddr exp))))
		 (define-variable!
		   (cadr exp)
		   (mc-eval analyzed-value env) env)))))

	((set-exp? exp)
	 (let ((analyzed-value (analyze (caddr exp))))
	   (lambda (env)
	     (set-variable-value! (cadr exp) (mc-eval analyzed-value env) env))))
	
	((begin-exp? exp)
	 (lambda (env)
	   (let ((analyzed-exps (map analyze (cdr exp))))
	     (eval-sequence analyzed-exps env))))
	 
	((lambda-exp? exp)
	 (lambda (env)
	   (let ((analyzed-body (map analyze (cddr exp))))
	     (make-procedure (cadr exp) analyzed-body env))))
	 
	((list? exp) (transform-proc-call exp))
	(else (error "UNKNOWN expression"))))


(define (transform-proc-call exp)
  (let ((analyzed-fn (analyze (car exp)))
	(analyzed-args (map analyze (cdr exp))))
    (lambda (env)
      (mc-apply (mc-eval analyzed-fn env)
		(map (lambda (arg)
		       (mc-eval arg env)) analyzed-args)))))
	   

(define (transformed? exp) (procedure? exp))


(define (mc-eval exp env)
  (exp env))


(define (mc-apply fn args)
  (cond ((lambda-proc? fn)
	  (eval-sequence (body fn)
			 (extend-environment
			     (params fn)
			     args
			     (env fn))))
       (else (do-magic fn args))))

;;;;;;;;;;;;;;;;;;;
;; Procedure ADT ;;
;;;;;;;;;;;;;;;;;;;

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


(define (eval-sequence exps env)
  (cond ((null? (cdr exps)) (mc-eval (car exps) env))
	(else
	  (mc-eval (car exps) env)
	  (eval-sequence (cdr exps) env))))


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
	       '=   '<    'list 'length)
	(list + - / * car cdr cons new-null? nil yell square factorial
	      new-= new-<  list  length   )))


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

(input-loop)
