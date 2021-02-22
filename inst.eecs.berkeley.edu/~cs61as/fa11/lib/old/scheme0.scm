;; Simple evaluator for Scheme without DEFINE, using substitution model.
;; Version 0: Primitive procedures aren't first-class.

;; The "read-eval-print loop" (REPL):

(define (scheme-0)
  (display "Scheme-0: ")
  (flush)
  (print (eval-0 (read)))
  (scheme-0))

;; Two important procedures:
;; EVAL-0 takes an expression and returns its value.
;; APPLY-0 takes a procedure and a list of actual argument values, and
;;  calls the procedure.
;; They have these names to avoid conflict with STk's EVAL and APPLY,
;;  which have similar meanings.

;; Comments on EVAL-0:

;; There are four basic expression types in Scheme:
;;    1. self-evaluating (a/k/a constant) expressions: numbers, #t, etc.
;;    2. symbols (variables)
;;    3. special forms (in this evaluator, just QUOTE, IF, and LAMBDA)
;;    4. procedure calls (can call a primitive or a LAMBDA-generated procedure)

;; 1.  The value of a constant is itself.

;; 2.  In the substitution model, we should never actually evaluate a *local*
;; variable name, because we should have substituted the actual value for
;; the parameter name before evaluating the procedure body.

;; In this simple evaluator, there is no DEFINE, and so the only *global*
;; symbols are the ones representing primitive procedures.  In this first
;; evaluator, such symbols can't be used as expressions in themselves,
;; but are legal only as the first element of a procedure call expression.

;; Therefore, symbols are illegal expressions in this interpreter.

;; 3.  The value of the expression (QUOTE FOO) is FOO -- the second element of
;; the expression.

;; To evaluate the expression (IF A B C) we first evaluate A; then, if A is
;; true, we evaluate B; if A is false, we evaluate C.

;; The value of a LAMBDA expression is the expression itself.  There is no
;; work to do until we actually call the procedure.  (This won't be true
;; when we write a more realistic interpreter that handles more Scheme
;; features, but it works in the substitution model.)

;; 4.  To evaluate a procedure call, we recursively evaluate the argument
;; subexpressions.  We call APPLY-0 to handle the actual procedure invocation.

(define (eval-0 exp)
  (cond ((constant? exp) exp)
	((symbol? exp) (error "Free variable: " exp))
	((quote-exp? exp) (cadr exp))
	((if-exp? exp)
	 (if (eval-0 (cadr exp))
	     (eval-0 (caddr exp))
	     (eval-0 (cadddr exp))))
	((lambda-exp? exp) exp)
	((pair? exp) (apply-0 (car exp)
			      (map eval-0 (cdr exp))))
	(else (error "bad expr: " exp))))


;; Comments on APPLY-0:

;; There are two kinds of procedures: primitive and LAMBDA-created.

;; In this evaluator, primitive procedures are represented by
;; their names.  To call a primitive, we use STk's EVAL procedure to
;; get the actual procedure, and STk's APPLY procedure to invoke it.

;; If the procedure isn't primitive, then it must be LAMBDA-created.
;; In this interpreter (but not in later, more realistic ones), the value
;; of a LAMBDA expression is the expression itself.  So (CADR PROC) is
;; the formal parameter list, and (CADDR PROC) is the expression in the
;; procedure body.

;; To call the procedure, we must substitute the actual arguments for
;; the formal parameters in the body; the result of this substitution is
;; an expression which we can then evaluate with EVAL-0.

(define (apply-0 proc args)
  (cond ((symbol? proc)		; use underlying Scheme's EVAL and APPLY
	 (apply (eval proc) args))
	((lambda-exp? proc)
	 (eval-0 (substitute (caddr proc)   ; the body
			     (cadr proc)    ; the formal parameters
			     args           ; the actual arguments
			     '())))
	(else (error "bad proc: " proc))))


;; Some trivial helper procedures:

(define (constant? exp)
  (or (number? exp) (boolean? exp) (string? exp)))

(define (exp-checker type)
  (lambda (exp) (and (pair? exp) (eq? (car exp) type))))

(define quote-exp? (exp-checker 'quote))
(define if-exp? (exp-checker 'if))
(define lambda-exp? (exp-checker 'lambda))


;; SUBSTITUTE substitutes actual arguments for *free* references to the
;; corresponding formal parameters.  For example, given the expression
;;
;;	((lambda (x y)
;;	   ((lambda (x) (+ x y))
;;	    (* x y)))
;;	 5 8)
;;
;; the body of the procedure we're calling is
;;
;;	   ((lambda (x) (+ x y))
;;	    (* x y))
;;
;; and we want to substitute 5 for X and 8 for Y, but the result should be
;;
;;	   ((lambda (x) (+ x 8))
;;	    (* 5 8))
;;
;; and *NOT*
;;
;;	   ((lambda (5) (+ 5 8))
;;	    (* 5 8))
;;
;; The X in (* X Y) is a "free reference," but the X in (LAMBDA (X) (+ X Y))
;; is a "bound reference."
;;
;; To make this work, in its recursive calls, SUBSTITUTE keeps a list of
;; bound variables in the current subexpression -- ones that shouldn't be
;; substituted for -- in its argument BOUND.  This argument is the empty
;; list in the top-level call to SUBSTITUTE from APPLY-0.

;; Another complication is that when an argument value isn't a self-evaluating
;; expression, we actually want to substitute the value *quoted*.  For example,
;; consider the expression
;;
;;	((lambda (x) (first x)) 'foo)
;;
;; The actual argument value is FOO, but we want the result of the
;; substitution to be
;;
;;	(first 'foo)
;;
;; and not
;;
;;	(first foo)
;;
;; because what we're going to do with this expression is try to evaluate
;; it, and FOO would be an unbound variable.

(define (substitute exp params args bound)
  (cond ((constant? exp) exp)
	((symbol? exp)
	 (if (memq exp bound)
	     exp
	     (lookup exp params args)))
	((quote-exp? exp) exp)
	((lambda-exp? exp)
	 (list 'lambda
	       (cadr exp)
	       (substitute (caddr exp) params args (append bound (cadr exp)))))
	(else (map (lambda (subexp) (substitute subexp params args bound)) exp))))

(define (lookup name params args)
  (cond ((null? params) name)
	((eq? name (car params)) (maybe-quote (car args)))
	(else (lookup name (cdr params) (cdr args)))))

(define (maybe-quote value)
  (cond ((lambda-exp? value) value)
	((constant? value) value)
	(else (list 'quote value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample evaluation, computing factorial of 5:

; Scheme-0: ((lambda (n)
;	       ((lambda (f) (f f n))
;		(lambda (f n)
;		   (if (= n 0)
;		       1
;		       (* n (f f (- n 1))) )) ))
;	     5)
; 120
