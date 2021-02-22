;;;;Nondeterministic evaluator
;;;;Different from the one in chapter 4 of SICP, in that it's based on the
;;;; vanilla metacircular evaluator, rather than on the analyzing one.

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

(define (ambeval exp env succeed fail)
  (cond ((self-evaluating? exp) (succeed exp fail))
	((variable? exp)
	 (succeed (lookup-variable-value exp env)
		  fail))
	((quoted? exp) (succeed (text-of-quotation exp) fail))
	((assignment? exp) (eval-assignment exp env succeed fail))
	((definition? exp) (eval-definition exp env succeed fail))
	((if? exp) (eval-if exp env succeed fail))
	((lambda? exp)
	 (succeed (make-procedure (lambda-parameters exp)
				  (lambda-body exp)
				  env)
		  fail))
	((begin? exp) 
	 (eval-sequence (begin-actions exp) env succeed fail))
	((cond? exp) (ambeval (cond->if exp) env succeed fail))
        ((let? exp) (ambeval (let->combination exp) env succeed fail)) ;**
        ((amb? exp) (eval-amb exp env succeed fail))                   ;**
	((application? exp)
	 (eval-application exp env succeed fail))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (eval-application exp env succeed fail)
  (ambeval (operator exp)
	   env
	   (lambda (proc fail2)
	     (get-args (operands exp)
		       env
		       (lambda (args fail3)
			 (execute-application proc args succeed fail3))
		       fail2))
	   fail))

(define (get-args exps env succeed fail)
  (if (null? exps)
      (succeed '() fail)
      (ambeval (car exps)
	       env
	       (lambda (arg fail2)
		 (get-args (cdr exps)
			   env
			   (lambda (args fail3)
			     (succeed (cons arg args)
				      fail3))
			   fail2))
	       fail)))

(define (execute-application procedure arguments succeed fail)
  (cond ((primitive-procedure? procedure)
         (succeed (apply-primitive-procedure procedure arguments) fail))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))
	  succeed
	  fail))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (eval-if exp env succeed fail)
  (ambeval (if-predicate exp)
	   env
	   (lambda (pred-value fail2)
	     (if (true? pred-value)
		 (ambeval (if-consequent exp)
			  env
			  succeed
			  fail2)
		 (ambeval (if-alternative exp)
			  env
			  succeed
			  fail2)))
	   fail))

(define (eval-sequence exps env succeed fail)
  (define (loop first-exp rest-exps succeed fail)
    (if (null? rest-exps)
        (ambeval first-exp env succeed fail)
	(ambeval first-exp
		 env
		 (lambda (first-value fail2)
		   (loop (car rest-exps) (cdr rest-exps) succeed fail2))
		 fail)))
  (if (null? exps)
      (error "Empty sequence")
      (loop (car exps) (cdr exps) succeed fail)))

(define (eval-definition exp env succeed fail)
  (ambeval (definition-value exp)
	   env
	   (lambda (val fail2)
	     (define-variable! (definition-variable exp) val env)
	     (succeed 'ok fail2))
	   fail))

(define (eval-assignment exp env succeed fail)
  (ambeval (assignment-value exp)
	   env
	   (lambda (val fail2)
	     (let* ((var (assignment-variable exp))
		    (old-value
		     (lookup-variable-value var env)))
	       (set-variable-value! var val env)
	       (succeed 'ok
			(lambda ()
			  (set-variable-value! var old-value env)
			  (fail2)))))
	   fail))


(define (eval-amb exp env succeed fail)
  (define (try-next choices)
    (if (null? choices)
	(fail)
	(ambeval (car choices)
		 env
		 succeed
		 (lambda ()
		   (try-next (cdr choices))))))
  (try-next (amb-choices exp)))


;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
	(else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'import
                      (list 'primitive
			    (lambda (name)
			      (define-variable! name
				                (list 'primitive (eval name))
				                the-global-environment)))
                      initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; Support for Let (as noted in footnote 56, p.428)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))
                     
;; A longer list of primitives -- suitable for running everything in 4.3
;; Overrides the list in ch4-mceval.scm
;; Has Not to support Require; various stuff for code in text (including
;;  support for Prime?); integer? and sqrt for exercise code;
;;  eq? for ex. solution

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
	(list 'append append)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
	(list '/ /)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list '< <)
        (list '<= <=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'quotient quotient)
        (list 'number? number?)
        (list 'integer? integer?)
        (list 'boolean? boolean?)
        (list 'string? string?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
	(list 'equal? equal?)
	(list 'se se)
	(list 'sentence se)
	(list 'first first)
	(list 'butfirst bf)
	(list 'bf bf)
        (list 'display display)
;;      more primitives
        ))

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
;;(define the-global-environment (setup-environment))
;;(driver-loop)

;; Added at Berkeley:
(define the-global-environment '())

(define (mce)
  (set! the-global-environment (setup-environment))
  (ambeval '(define (require p) (if (not p) (amb)))
	   the-global-environment
	   (lambda (a b) #t)
	   (lambda () #t))
  (driver-loop))


(define (trace-me)
  (trace ambeval)
  (trace eval-assignment)
  (trace eval-if)
  (trace eval-amb)
  'okay)
