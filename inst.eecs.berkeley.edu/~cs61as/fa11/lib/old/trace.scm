;; TRACE, modified to work with the MCE
;;
;; New syntax is (TRACE PROC INPUT-PRINTER OUTPUT-PRINTER)
;;
;; INPUT-PRINTER and OUTPUT-PRINTER are optional arguments that
;; override the default routines to display parameters and return
;; values from the given traced procedure.  They both take one
;; parameter and default to WRITE*, which knows about circular
;; structures.
;;
;; IMPORTANT:
;; You must load this file before you load mceval.scm!  To trace MC-EVAL and
;; MC-APPLY without displaying environments at all, enter the following:
;;
;; (trace mc-eval mce-pretty-print mce-pretty-print)
;; (trace mc-apply mce-pretty-print mce-pretty-print)


;; TRACE, UNTRACE
;;
;; STk doesn't have weak pairs, so to support untracing, the old
;; procedure is bound in the current frame rather than in a global
;; list.  There's also a special binding for the traced version of the
;; procedure, which is used as an error check in case the procedure is
;; redefined.
;;
;; These are implemented as macros so we can define things in the
;; current frame, and so we can grab the name of the procedure we're
;; tracing.

(define-macro (trace . args)
  (let ((proc (car args))
	(input-printer (and (pair? (cdr args)) (cadr args)))
	(output-printer (and (pair? (cdr args)) (pair? (cddr args)) (caddr args))))
    `(if (and (symbol-bound? ',(traced-proc-symbol proc))
	      (eq? ,(traced-proc-symbol proc) ,proc))
	 (error "trace: procedure is already traced: " ',proc)
	 (begin
	   (define ,(old-proc-symbol proc) ,proc)
	   (define ,proc (trace-it ,proc ',proc ,input-printer ,output-printer))
	   (define ,(traced-proc-symbol proc) ,proc)))))

(define-macro (untrace proc)
  `(if (and (symbol-bound? ',(traced-proc-symbol proc))
	    (eq? ,(traced-proc-symbol proc) ,proc))
       (begin
	 (set! ,proc ,(old-proc-symbol proc))
	 (set! ,(traced-proc-symbol proc) '()))
       (error "untrace: procedure is not traced: " ',proc)))

(define-macro (old-proc-symbol procname)
  `(word '*old- ,procname '*))

(define-macro (traced-proc-symbol procname)
  `(word '*traced- ,procname '*))


;; TRACE-IT
;;
;; Takes a procedure and other useful information as arguments and returns
;; a traced version of the procedure.

(define trace-it
  (let ((indent 0) (procedure-body procedure-body))
    (define (dots n)
      (if (> n 0) (begin (display ".") (dots (1- n)))))
    (lambda (proc procname input-printer output-printer)
      (if (not proc)  ;; special message to reset indent after error
	  (set! indent 0)
	  (let* ((body (procedure-body proc))
		 (formals (if (pair? body) (cadr body) '#0=(arg . #0#))))
	    
	    ;; PRINT-PARAMS--output formal params and actual argument values
	    (define (print-params formals actuals)
	      (define pretty-bindings
		(let ((already-run? #f))
		  (lambda (formal actual)
		    (if already-run?
			(display ", ")
			(begin (set! already-run? #t) (display " with ")))
		    (show-binding formal actual))))
	      (define (show-binding formal actual)
		(display formal) (display " = ")
		((or input-printer write*) actual))
	      (dots indent) (display " -> ") (display procname)
	      (for-each pretty-bindings formals actuals)
	      (newline))
	    
	    ;; PRINT-RETURN--output return value
	    (define (print-return val)
	      (define (show-return val)
		((or output-printer write*) val))
	      (dots indent) (display " <- ") (display procname)
	      (display " returns ") (show-return val) (newline))
	    
	    ;; the traced procedure returned by TRACE
	    (lambda args
	      (set! indent (+ indent 2))
	      (print-params formals args)
	      (let ((result (apply proc args)))
		(print-return result)
		(set! indent (- indent 2))
		result)))))))


;; REPL hook
;;
;; When an error occurs, INDENT needs to be reset to 0.  We accomplish
;; this by passing #f as the first argument to TRACE-IT.  Note that
;; this should be a hook for errors, but that doesn't work right in
;; STk, so we hook the REPL instead.

(define repl-display-prompt
  (let ((old-repl-display-prompt repl-display-prompt))
    (lambda args
      (trace-it #f #f #f #f)
      (apply old-repl-display-prompt args))))


;; MCE-PRETTY-PRINT
;;
;; An inefficient printer procedure for tracing the MCE.  (No, it
;; technically shouldn't be called a ``pretty printer.'')  This
;; procedure tries to identify environments and avoid printing them
;; entirely.  The algorithm is naive: it considers any list whose last
;; pair is the global environment to be an environment.

(define (mce-pretty-print exp)
  (display (make-pretty exp)))

(define mpp mce-pretty-print)

(define (make-pretty exp)
  (define (environment-representation env)
    (let ((len (length env)))
      (case len
	((1) "#[global]")
	((2) "#[env extends global]")
	(else (string-append "#[env depth=" (number->string (1- len)) "]")))))
  (cond ((environment? exp) (environment-representation exp))
	((pair? exp) (cons (make-pretty (car exp)) (make-pretty (cdr exp))))
	(else exp)))

(define (environment? exp)  
  (and (pair? exp) (list? exp) (eq? (last-pair exp) the-global-environment)))
