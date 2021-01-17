
;;; Demonstration program for substitution-model evaluation
;;;   complete with normal-order and applicative-order versions.
;;; For use with Section 1.1 of SICP.
;;;
;;; This file defines three special forms: DEF, APPLIC, and NORMAL.
;;;
;;; Example of use:
;;;
;;; (def (f a b) (+ (g a) b))     ; define a function
;;; (def (g x) (* 3 x))           ; another one
;;; (applic (f (+ 2 3) (- 15 6))) ; show applicative-order evaluation
;;; (normal (f (+ 2 3) (- 15 6))) ; show normal-order evaluation

;;; In the printed results, something like
;;;     (* 2 3) ==> 6
;;; indicates the ultimate invocation of a primitive function.  But
;;;     (f 5 9) ---->
;;;     (+ (g 5) 9)
;;; indicates the substitution of actual arguments into the body of
;;; a function defined with DEF.  (Of course, whether actual argument
;;; values or actual argument expressions are substituted depends on
;;; whether you used APPLIC or NORMAL, respectively.)

;;; Restrictions:
;;;   * The operands of a combination must be numbers or combinations.
;;;          (I.e., no variables, no non-numeric data.)
;;;   * The operator of a combination must be a symbol that names a function.
;;;          If the function was not defined by DEF, it is taken as primitive
;;;          for purposes of the substitution model.
;;;   * DEF only understands the short-form function definition syntax.
;;;          (I.e., no lambda, no defining anything but functions.)
;;;   * The body of a function defined with DEF must be exactly one expression.

;;; A neat example:
;;; (def (zero x) (- x x))
;;; (applic (zero (random 10)))
;;; (normal (zero (random 10)))


;; The DEF special form.
;; Binds the symbol to a quoted lambda expression, not a closure, since we're
;;  using the substitution model.

;(define def (procedure->macro
;	     (lambda (exp env)
;	       `(begin (define ,(caadr exp)
;			 '(lambda ,(cdadr exp) ,(caddr exp)))
;		       (set! def-names (cons ',(caadr exp) def-names))
;		       ',(caadr exp)))))

(define-macro (def form . body) 
  `(begin
    (define ,(car form) '(lambda ,(cdr form) ,@body))
    (set! def-names (cons ',(car form) def-names))
    ',(car form)))

;;;; (extend-syntax (def)
;;;;   [(def (name . args) body)
;;;;    (begin (define name '(lambda args body))
;;;; 	  (set! def-names (cons 'name def-names))
;;;; 	  'name)])

;; A list of the functions defined using DEF.
;; We look in here to distinguish defined functions from primitives.

(define def-names '())

;; The APPLIC special form.  Expands an expression in applicative order.
;; Calls procedure applic1 to do the real work, except for some extra
;; top-level stuff to keep the return value separate from printed text.

;(define applic (procedure->macro
;		(lambda (exp env)
;		  `(let ((result (applic1 ',(cadr exp) "")))
;		     (newline)
;		     result))))

(define-macro (applic . exp)
   `(let ((result (applic1 ',(car exp) "")))
      (newline)
      result))

;;;; (extend-syntax (applic)
;;;;   [(applic thingo)
;;;;    (let ((result (applic1 'thingo '||)))
;;;;      (newline)
;;;;      result)])

;; The second argument to applic1 is a word of (initially zero) spaces
;; used to indent the printing of the expansion of subexpressions.

(define (applic1 form spaces)
  (if (not (pair? form))
      form
      (begin
       (newline)
       (display spaces)
       (display form)
       (cond ((and (not (memq (car form) def-names))
		   (all-numbers? (cdr form)))
	      (display " ==> ")
	      (let ((ans (eval form)))
		(display ans)
		ans))
 	     (else
	      (let ((new-form (subapplic (list (car form))
					 (cdr form)
					 (word spaces "   ") )))
		(if (and (memq (car form) def-names)
			 (not (all-numbers? (cdr form))) )
		    (begin (newline) (display spaces) (display new-form)) )
		(cond ((memq (car form) def-names)
		       (display " ----> ")
		       (applic1 (subst (eval (car form)) (cdr new-form))
				spaces))
		      ((equal? (car form) 'quote) (cadr form))
		      (else (applic1 new-form spaces)) )))))))

(define (all-numbers? l)
  (cond ((null? l) #t)
	((not (number? (car l))) #f)
	(else (all-numbers? (cdr l))) ))

;; subapplic maps applic1 over the operands, left-to-right.

(define (subapplic done todo spaces)
  (if (null? todo)
      (reverse done)
      (let ((result (applic1 (car todo) spaces)))
	(subapplic (cons result done) (cdr todo) spaces) )))

;; subst takes a lambda expression and an actual argument list, and
;; returns the body with substitutions of args for formal parameters.

(define (subst proc args)
  (subst-in-body (caddr proc) (cadr proc) args))

(define (subst-in-body form params args)
  (cond ((null? form) '())
	((not (pair? form)) (lookup form params args))
	(else (cons (subst-in-body (car form) params args)
		    (subst-in-body (cdr form) params args) ))))

(define (lookup form params args)
  (cond ((null? params) form)
	((eq? form (car params)) (car args))
	(else (lookup form (cdr params) (cdr args))) ))

;; The NORMAL special form.  Everything below here is analogous to the
;; corresponding piece of APPLIC, but the logic of normal1 is different.

;(define normal (procedure->macro
;		(lambda (exp env)
;		  `(let ((result (normal1 ',(cadr exp) "")))
;		     (newline)
;		     result))))

(define-macro (normal . exp)
	`(let ((result (normal1 ',(car exp) "")))
	    (newline)
	    result))

;;;; (extend-syntax (normal)
;;;;   [(normal thingo)
;;;;    (let ((result (normal1 'thingo "")))
;;;;      (newline)
;;;;      result)])

(define (normal1 form spaces)
  (if (not (pair? form))
      form
      (begin
       (newline)
       (display spaces)
       (display form)
       (cond ((and (not (memq (car form) def-names))
		   (all-numbers? (cdr form)))
	      (display " ==> ")
	      (let ((ans (eval form)))
		(display ans)
		ans))
 	     ((memq (car form) def-names)
		       (display " ----> ")
		       (normal1 (subst (eval (car form)) (cdr form))
				spaces))
	     ((equal? (car form) 'quote) (cadr form))
	     (else
	      (let ((new-form (subnormal (list (car form))
					 (cdr form)
					 (word spaces "   ") )))
		(normal1 new-form spaces) ))))))

(define (subnormal done todo spaces)
  (if (null? todo)
      (reverse done)
      (let ((result (normal1 (car todo) spaces)))
	(subnormal (cons result done) (cdr todo) spaces) )))

