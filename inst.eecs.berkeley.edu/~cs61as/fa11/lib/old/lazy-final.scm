;; Lazy Evaluator

(define (input-loop)
  (display "=61A(Lazy)=> ")
  (flush)
  
  (let ((input (read)))
    (if (equal? input 'exit)
	(print "Au Revoir!")
	(begin
	  (print (ACTUAL-VALUE (mc-eval input the-global-env))) ;; NEW!!
          (input-loop)))))


(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((if-exp? exp)
	 (if (not (eq? (ACTUAL-VALUE (mc-eval (cadr exp) env)) 'nay)) ;; NEW!!
	     (mc-eval (caddr exp) env)
	     (mc-eval (cadddr exp) env)))
	((begin-exp? exp)
	 (eval-sequence (cdr exp) env))
	((quote-exp? exp) (cadr exp))
	((set-exp? exp)
	     (set-variable-value! (cadr exp)
				  (mc-eval (caddr exp) env)
				  env))
	((definition? exp)
	 (if (list? (cadr exp))
	     (mc-eval (define->lambda exp) env)
	     (define-variable!
			     (cadr exp)
			     (mc-eval (caddr exp) env) env)))
	
	((lambda-exp? exp) (make-procedure (cadr exp) (cddr exp) env))
	((list? exp) (mc-apply (ACTUAL-VALUE (mc-eval (car exp) env)) ;; NEW!!
			      (map (lambda (arg-exp)
				     (make-thunk arg-exp env)) (cdr exp))))
	(else (error "UNKNOWN expression"))))



(define (mc-apply fn args)
  (cond ((lambda-proc? fn)
	  (eval-sequence (body fn)
			 (extend-environment
			     (params fn)
			     args
			     (env fn))))
       (else (do-magic fn (map ACTUAL-VALUE args))))) ;; NEW!!

;;;;;;;;;;;;;;;;;;;;;;
;; Lazy Evaluator Helper
;;;;;;;;;;;;;;;;;;;;;

(define (make-thunk exp env)
  (list 'thunk exp env))

(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

(define (thunk? arg)
  (and (list? arg)
       (eq? (car arg) 'thunk)))

(define (actual-value arg)
  (if (thunk? arg)
      (let ((val (mc-eval (thunk-exp arg)
			  (thunk-env arg))))
	(actual-value val))
      arg))

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
