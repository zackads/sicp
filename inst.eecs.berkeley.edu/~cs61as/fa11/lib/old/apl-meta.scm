;;; apl-meta.scm     APL version of metacircular evaluator.

;;; SETTING UP THE ENVIRONMENT

;;; APL primitives aren't part of the environment.  They are not subject
;;; to redefinition, for example.  They are kept in a separate list.  So
;;; the initial environment is empty.  But define! only works if there is
;;; a non-empty environment, so we fake something.

(define the-global-environment '())

;;; INITIALIZATION AND DRIVER LOOP

;;; The following code initializes the machine and starts the APL
;;; system.  You should not call it very often, because it will clobber
;;; the global environment, and you will lose any definitions you have
;;; accumulated.

(define (initialize-apl)
  (set! the-global-environment
  	(extend-environment '(no-name) '(no-value) '()))
  (set! apl-operators
    (list (make-scalar-op '+ (lambda (x) x) +)
	  (make-scalar-op '- - -)
	  (make-scalar-op '*
		 	  (lambda (x) (cond ((< x 0) -1) ((= x 0) 0) (else 1)))
		 	  *)
	  (make-scalar-op '% / /)
	  (make-scalar-op 'bar abs rem)
	  (make-scalar-op '= error (apl-pred2 =))
	  (make-scalar-op '< error (apl-pred2 <))
	  (make-scalar-op '> error (apl-pred2 >))
	  (make-op '/ error compress)
	  (make-op 'iota iota error)
	  (make-op 'rho shape reshape)
	  (make-op 'comma ravel cat)
	  (make-op 'gets error 'set!)))
  (apl-loop))

;;; APPLYING PRIMITIVE PROCEDURES

;;; The mechanism for applying primitive procedures is somewhat
;;; different from the one given in the course notes.  We can recognize
;;; primitive procedures (which are all inherited from Scheme) by asking
;;; Scheme if the object we have is a Scheme procedure.

(define (primitive-procedure? p)
  (applicable? p))

;;; To apply a primitive procedure, we ask the underlying Scheme system
;;; to perform the application.  (Of course, an implementation on a
;;; low-level machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)
  (apply p args))


;;; Now for the code from the book!!!


;;; Section 4.1.1

(define (mini-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((application? exp)
         (mini-apply (mini-eval (operator exp) env)
                     (list-of-values (operands exp) env)
		     env))
        (else (error "Unknown expression type -- MINI-EVAL" exp))))

(define (mini-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                         (parameters procedure)
                         arguments
                         env)))
        (else
         (error "Unknown procedure type -- MINI-APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (mini-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps)
                                    env)))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mini-eval (first-exp exps) env))
        (else (mini-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (let ((new-value (mini-eval (assignment-value exp) env)))
    (set-variable-value! (assignment-variable exp)
                         new-value
                         env)
    new-value))

;;; Section 4.1.2 -- Representing expressions

;;; numbers

(define (self-evaluating? exp) (number? exp))

;;; variables

(define (variable? exp) (symbol? exp))

;;; assignment

(define (assignment? exp)
  (if (not (pair? exp))
      #f
      (eq? (car exp) 'set!)))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;; sequences

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

;;; procedure applications

(define (application? exp)
  (if (not (pair? exp))
      #f
      (procedure? (car exp))))

(define (procedure? exp)
  (or (applicable? exp) (compound-procedure? exp)))

(define (operator app) (car app))

(define (operands app) (cdr app))

(define (no-operands? args) (null? args))

(define (first-operand args) (car args))

(define (rest-operands args) (cdr args))

;;; Representation of procedure objects

(define (make-procedure lambda-exp env)
  (list 'procedure lambda-exp env))

(define (compound-procedure? proc)
  (if (not (pair? proc))
      #f
      (eq? (car proc) 'procedure)))

(define (parameters proc) (cadr (cadr proc)))

(define (procedure-body proc) (cddr (cadr proc)))

(define (procedure-environment proc) (caddr proc))

;;; Section 4.1.3

;;; Operations on environments

(define (lookup-variable-value var env)
  (if (assq var apl-operators)
      var
      (let ((b (binding-in-env var env)))
	(if (found-binding? b)
	    (binding-value b)
	    (error "Unbound variable" var)))))

(define (binding-in-env var env)
  (if (no-more-frames? env)
      no-binding
      (let ((b (binding-in-frame var (first-frame env))))
        (if (found-binding? b)
            b
            (binding-in-env var (rest-frames env))))))

(define (extend-environment variables values base-env)
  (adjoin-frame (make-frame variables values) base-env))

(define (set-variable-value! var val env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (set-binding-value! b val)
        (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((b (binding-in-frame var (first-frame env))))
    (if (found-binding? b)
        (set-binding-value! b val)
        (set-first-frame!
          env
          (adjoin-binding (make-binding var val)
                          (first-frame env))))))

;;; Representing environments

(define (first-frame env) (car env))

(define (rest-frames env) (cdr env))

(define (no-more-frames? env) (null? env))

(define (adjoin-frame frame env) (cons frame env))

(define (set-first-frame! env new-frame)
  (set-car! env new-frame))

;;; Representing frames

(define (make-frame variables values)
  (cond ((and (null? variables) (null? values)) '())
        ((null? variables)
         (error "Too many values supplied" values))
        ((null? values)
         (error "Too few values supplied" variables))
        (else
         (cons (make-binding (car variables) (car values))
               (make-frame (cdr variables) (cdr values))))))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (assq key bindings)
  (cond ((null? bindings) no-binding)
        ((eq? key (binding-variable (car bindings))) 
         (car bindings))
        (else (assq key (cdr bindings)))))

(define (binding-in-frame var frame)
  (assq var frame))

(define (found-binding? b)
  (not (eq? b no-binding)))

(define no-binding '())

;;; Representing bindings

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding value)
  (set-cdr! binding value))
