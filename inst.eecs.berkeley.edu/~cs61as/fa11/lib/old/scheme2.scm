;; Simple evaluator for Scheme, introducing environment model.
;; Version 2: Adds DEFINE to Scheme-1.

;; In order to remember definitions, we need something new: *environments*.
;; An environment is a collection of names and associated values.  We'll
;; represent an environment as a table.  (You'll see in the homework that
;; this isn't quite good enough, but it works in almost all situations.)

(DEFINE THE-GLOBAL-ENVIRONMENT (LIST '*TABLE*))

;; EVAL-2 now requires two arguments: the expression to evaluate, and an
;; environment in which to find variables.  This changes the REPL.  Also,
;; instead of just PRINT we use a magic incantation of FORMAT that lets
;; PRINT print circular structures without getting in an infinite
;; loop, because someone might try to print a procedure.

(define (scheme-2)
  (display "Scheme-2: ")
  (flush)
  (print (format #f "~w" (eval-2 (read) THE-GLOBAL-ENVIRONMENT)))
  (scheme-2))

;; Comments on EVAL-2 vs. EVAL-1:

;; EVAL-2 takes the current environment as an argument.

;; Here are the four expression types:
;;    1. self-evaluating (a/k/a constant) expressions: numbers, #t, etc.
;;    2. symbols (variables)
;;    3. special forms (in this evaluator, just QUOTE, IF, LAMBDA, and DEFINE)
;;    4. procedure calls (can call a primitive or a LAMBDA-generated procedure)

;; 1.  No change to handling of self-evaluating expressions.  They don't
;; have to refer to the environment because their values are constant.
;; Since we aren't using substitution, we can remove the kludge in scheme-1
;; wherein an STk procedure is considered a self-evaluating expression.

;; 2.  Now we have local variables, so we look up variables in the
;; current environment, which is a table.  If we don't find the name in
;; the environment, we continue to cheat by asking STk for a global
;; value, so that we inherit all the Scheme primitives.

;; 3.  The handling of QUOTE is unchanged.

;; IF is almost unchanged, but the recursive calls to EVAL-2 require
;; the environment as a second argument.

;; LAMBDA is handled differently, because a procedure is now something
;; more than just its defining text.  It must also remember the
;; environment in which it was created (the right bubble in SICP
;; environment diagrams).

;; To handle internal definitions, we have to allow more than one
;; expression in a procedure body.  (This affects APPLY-2.)

;; Don't try to print SCHEME-2 procedures in STk!  The data structure
;; that represents the procedure is circular.  (It includes the environment,
;; which generally includes a binding whose value is the procedure.)  But
;; you can use the magic incantation
;; 	(print (format #f "~w" foo))
;; to print circular structures without looping.

;; DEFINE is a new special form.  It extends the current environment with
;; a new name-value pair, using the PUT procedure for table updating.

;; Note that this DEFINE does not handle the shortcut notation for
;; defining procedures!  You have to say
;;	(define foo (lambda (x) ...))
;; and not
;;	(define (foo x) ...)

;; 4.  Procedure calling is unchanged except that the recursive calls to
;; EVAL-2 require the environment as an argument.  (But when we get to APPLY-2
;; we'll have to remember that a procedure isn't just a lambda expression.)

(define (eval-2 exp ENV)				; second arg is env
  (cond ((constant? exp) exp)
	((symbol? exp) (LOOKUP EXP ENV))		; look in environment
	((quote-exp? exp) (cadr exp))
	((if-exp? exp)
	 (if (eval-2 (cadr exp) ENV)
	     (eval-2 (caddr exp) ENV)
	     (eval-2 (cadddr exp) ENV)))
	((lambda-exp? exp) (MAKE-PROC EXP ENV))		; procs include env
	((DEFINE-EXP? EXP) (PUT (CADR EXP)		; the DEFINE form
				(EVAL-2 (CADDR EXP) ENV)
				ENV)
	 		   'OKAY)
	((pair? exp) (apply-2 (eval-2 (car exp) ENV)
			      (map (LAMBDA (E) (EVAL-2 E ENV)) (cdr exp))))
	(else (error "bad expr: " exp))))


;; Comments on APPLY-2 vs. APPLY-1:

;; Primitive procedure calling is unchanged.

;; But user-defined procedures are no longer invoked by substitution!
;; Instead, we just EVAL-2 the body *in a new environment* that we
;; create by adding bindings for the procedure's parameters to the
;; environment in which the procedure was created.

;; Note that EXTEND-ENVIRONMENT makes a new table, with a new header
;; pair, even though the new table starts with the same association
;; list as the environment we're extending.  This is important because
;; the new bindings we add should affect only this procedure call, not
;; the enclosing environment.

;; A procedure body is now a list of expressions, not just one
;; expression, so we invent EVAL-SEQUENCE to evaluate them.

(define (apply-2 proc args)
  (cond ((procedure? proc)	; use underlying Scheme's APPLY
	 (apply proc args))
	((LAMBDA-PROC? proc)
	 (EVAL-SEQUENCE (PROC-BODY PROC)
			(EXTEND-ENVIRONMENT (PROC-PARAMS PROC)
					    ARGS
					    (PROC-ENV PROC))))
	(else (error "bad proc: " proc))))

(DEFINE (EVAL-SEQUENCE EXPS ENV)
  (IF (NULL? (CDR EXPS))	; unusual base case so that we
      (EVAL-2 (CAR EXPS) ENV)	; return the value of the last expr
      (BEGIN (EVAL-2 (CAR EXPS) ENV)
	     (EVAL-SEQUENCE (CDR EXPS) ENV))))

(DEFINE (EXTEND-ENVIRONMENT PARAMS ARGS ENV)
  (LET ((NEW-ENV (CONS '*TABLE* (CDR ENV))))
    (DEFINE (EE-LOOP PARAMS ARGS)
      (IF (NULL? PARAMS)
	  NEW-ENV
	  (BEGIN (PUT (CAR PARAMS) (CAR ARGS) NEW-ENV)
		 (EE-LOOP (CDR PARAMS) (CDR ARGS)))))
    (EE-LOOP PARAMS ARGS)))


;; Some trivial helper procedures:

(define (constant? exp)
  (or (number? exp) (boolean? exp) (string? exp)))

(define (exp-checker type)
  (lambda (exp) (and (pair? exp) (eq? (car exp) type))))

(define quote-exp? (exp-checker 'quote))
(define if-exp? (exp-checker 'if))
(define lambda-exp? (exp-checker 'lambda))
(DEFINE DEFINE-EXP? (EXP-CHECKER 'DEFINE))


;; Data abstraction for procedures:

(DEFINE (MAKE-PROC EXP ENV)
  (LIST 'PROCEDURE EXP ENV))

(DEFINE LAMBDA-PROC? (EXP-CHECKER 'PROCEDURE))

(DEFINE PROC-LAMBDA CADR)
(DEFINE PROC-ENV CADDR)

(DEFINE (PROC-PARAMS PROC)
  (CADR (PROC-LAMBDA PROC)))

(DEFINE (PROC-BODY PROC)	; note CDDR not CADDR because
  (CDDR (PROC-LAMBDA PROC)))	; a body can be more than one expr


;; Environment (table) maintenance

;; PUT is different from the one in the book because if we add a new
;; entry with the same key as an old entry (a new variable with the
;; same name as an old one), we don't want to modify the old pair,
;; which might still be valid in an outer environment.  This will be
;; the case in a situation like
;;	(define (foo x)
;;	  (define (baz x)
;;	    ...)
;;	  ...)

(DEFINE (LOOKUP VAR ENV)
  (LET ((RESULT (ASSOC VAR (CDR ENV))))
    (IF RESULT
	(CDR RESULT)
	(EVAL VAR))))			; Not in env, ask STk

(DEFINE (PUT VAR VAL ENV)
  (SET-CDR! ENV (CONS (CONS VAR VAL) (CDR ENV))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample evaluation, using a primitive as argument to MAP:

; Scheme-2: (define map
; 	      (lambda (f seq)
; 	        (if (null? seq)
; 		    '()
; 		    (cons (f (car seq)) (map f (cdr seq))))))
; okay
; Scheme-2: (map first '(the rain in spain))
; (t r i s)
