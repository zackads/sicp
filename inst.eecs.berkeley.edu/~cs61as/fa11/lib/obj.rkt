#lang racket

;; obj.rkt version 1.0 8/23/2015
;; Revised for Racket by Rohin Shah
;; Based on obj.scm version 4.0 5/18/2000
;; Revised for STk by Brian Gaeke - removed scm and procedure->macro
;; Based on an implementation of the object-oriented syntax
;; By Matt Wright, based on a handout from MIT

(provide define-class ask instantiate usual)

(require (for-syntax racket/syntax))

;; define-for-syntax allows us to define functions that can be used at
;; macro compile time
;; Basically, this allows us to use get-class-var-stuff in order to
;; generate code in macros
;; This function takes in a piece of syntax and outputs two lists.
;; The first is a list of bindings.  The list is a normal list (not a
;; syntax object).  Each binding is a syntax object of the form
;; (name value-expression)
;; The second is a list of cond clauses that create class-methods
;; (methods that return the values of class variables).
;; TODO: Explain why we want to return normal lists instead of syntax
;; objects of the form ((name1 value1) (name2 value2) ...)
(define-for-syntax (get-class-var-stuff stx)
  (syntax-case stx (class-vars)
    ;; Base case
    [()
     (values '() '())]
    ;; Case where we find the class-vars clause
    [((class-vars (name value) ...) clause ...)
     ;; Normally, right after the pattern match we would use a
     ;; (syntax/loc stx ...) in order to generate a piece of syntax,
     ;; and to be able to use the pattern-matched variables.
     ;; However, here we want to return multiple lists of values.
     ;; values allows us to return multiple values
     ;; syntax-e takes a syntax object and "unrolls" it into a list of
     ;; syntax objects.  For example, (syntax-e #'(+ 1 (+ 2 3))) gives
     ;; (list #'+ #'1 #'(+ 2 3)).
     ;; Finally, syntax/loc allows us to create a piece of syntax
     ;; using our pattern-matched variables.
     (values (syntax-e (syntax/loc stx ((name value) ...)))
             (syntax-e (syntax/loc stx ([(eq? message 'name)
                                         (lambda () name)] ...))))]
    ;; Recurse through the clauses:
    [(useless-clause clause ...)
     (get-class-var-stuff #'(clause ...))]))

;; Same thing as get-class-var-stuff except for instance-vars.
(define-for-syntax (get-instance-var-stuff stx)
  (syntax-case stx (instance-vars)
    [()
     '()]
    [((instance-vars (name value) ...) clause ...)
     (values (syntax-e (syntax/loc stx ((name value) ...)))
             (syntax-e (syntax/loc stx ([(eq? message 'name)
                                         (lambda () name)] ...))))]
    [(useless-clause clause ...)
     (get-instance-var-stuff #'(clause ...))]))

;; Similar to above
;; Returns a normal list of expressions in the initialize clause.
(define-for-syntax (get-initialize-expressions stx)
  (syntax-case stx (initialize)
    [()
     '()]
    [((initialize expr ...) clause ...)
     (syntax-e (syntax/loc stx (expr ...)))]
    [(useless-clause clause ...)
     (get-initialize-expressions #'(clause ...))]))

;; Similar to above
;; Returns a normal list of cond-clauses that become methods in the
;; generated code for the object
(define-for-syntax (get-methods stx)
  (syntax-case stx (method)
    [()
     '()]
    [((method (name arg ...) expr ...) clause ...)
     (cons (syntax/loc stx [(eq? message 'name)
                            (lambda (arg ...) expr ...)])
           (get-methods #'(clause ...)))]
    [(useless-clause clause ...)
     (get-methods #'(clause ...))]))

;; Similar to above
;; Returns code representing what the object should return when asked
;; for a method that doesn't exist (either a default method, or the
;; class name, which signals an error)
(define-for-syntax (get-default-method-code stx class-name)
  (syntax-case stx (default-method)
    [()
     ;; In the case where there is no default method, return the class
     ;; name.  However, this should have the same source code
     ;; information as stx, so use datum->syntax.
     ;; Source code information says where a piece of code came
     ;; from.  This allows macros to be hygienic - variables created
     ;; inside the macro have the "source code location" be the macro,
     ;; whereas variables in user code (the code that calls the macro)
     ;; has a different source code location and so can't be confused
     ;; with variables in the macro.
     (with-syntax ([name (datum->syntax stx (syntax->datum class-name))])
       (syntax/loc stx 'name))]
    [((default-method expr ...) clause ...)
     ;; We need to break hygiene for the args variable.
     ;; Specifically - the "user code" (i.e. code that uses
     ;; define-class) is allowed to use the "args" variable without
     ;; defining it.  The idea is that we define it for them by saying
     ;; (lambda args #,user-code).  However, if we do it just like
     ;; that, then our args will have a source code location of the
     ;; macro, which means it will be different from the args in the
     ;; user code.  Here, hygiene is actually annoying, and we want
     ;; unhygienic behavior - the "args" in our macro *is* the same as
     ;; the "args" in user code.
     ;; To fix this, we can use datum->syntax to construct an "args"
     ;; that has the same source code location as stx (which is user
     ;; code).  We then use this in our macro and it is identified as
     ;; the same args as in the user code, since it has the same
     ;; source code location.
     (with-syntax ([args-var (datum->syntax stx 'args)])
       (syntax/loc stx
         (lambda args-var expr ...)))]
    [(useless-clause clause ...)
     (get-default-method-code #'(clause ...) class-name)]))

;; TODO: Finish this and test it
(define-for-syntax (get-parent-stuff stx)
  (syntax-case stx (parent)
    [()
     (values '() '() '())]
    [((parent (name arg ...) ...) clause ...)
     ;(parent-ids parent-bindings initialize-parent-expressions)
     (let ([new-names (map (lambda (name-stx)
                             (format-id name-stx "my-~a" name-stx))
                           (syntax-e #'(name ...)))])
       (values new-names
               '()
               ;(syntax-e (syntax/loc stx ((name (instantiate-parent name arg ...)) ...)))
               ;; TODO: Return a list of pieces of code to initialize
               ;; the parents (see the use of
               ;; initialize-parent-expressions in define-class)
               '()))]
    [(useless-clause clause ...)
     (get-parent-stuff #'(clause ...))]))

;; The define-class macro
;; Most of the hard work is done in the previous helper functions.
;; We get various pieces of code from those helpers and then plug them
;; into the general template of a class (based on inspecting output
;; from obj.scm)
(define-syntax (define-class stx)
  (syntax-case stx ()
    [(_ (name instantiation-arg ...)
        clause ...)
     (let-values ([(class-var-bindings class-var-methods)
                   (get-class-var-stuff #'(clause ...))]
                  [(instance-var-bindings instance-var-methods)
                   (get-instance-var-stuff #'(clause ...))]
                  [(initialize-expressions)
                   (get-initialize-expressions #'(clause ...))]
                  [(methods)
                   (get-methods #'(clause ...))]
                  [(default-method-code)
                   (get-default-method-code #'(clause ...) #'name)]
                  [(parent-ids parent-bindings initialize-parent-expressions)
                   (get-parent-stuff #'(clause ...))]
                  ;; Use the lexical information from the user code
                  ;; for self, so that the self introduced by the
                  ;; macro is visible to user code.
                  [(self-var) (datum->syntax stx 'self)])
       #`(define name
           (let #,class-var-bindings
             (lambda (message)
               (cond [(eq? message 'class?)
                      #t]
                     #,@class-var-methods
                     [(eq? message (quote class-name))
                      (lambda () 'name)]
                     [(eq? message (quote instantiate))
                      (lambda (instantiation-arg ...)
                        (let ([#,self-var '()]
                              #,@parent-bindings
                              #,@instance-var-bindings)
                          (define (dispatch message)
                              (cond [(eq? message 'object?)
                                     #t]
                                    [(eq? message 'initialize)
                                     (lambda (value-for-self)
                                       (set! #,self-var value-for-self)
                                       #,@initialize-parent-expressions
                                       #,@initialize-expressions)]
                                    [(eq? message 'send-usual-to-parent)
                                     (lambda (message . args)
                                       ;; TODO
                                       (let ([method (get-method 'name message #,@parent-ids)])
                                         (if (procedure? method)
                                             (apply method args)
                                             (error (format "Method ~a not found in the parents of ~a" message 'name)))))]
                                    #,@methods
                                    [(eq? message 'class-name)
                                     (lambda () 'name)]
                                    [(eq? message 'instantiation-arg)
                                     (lambda () instantiation-arg)]
                                    ...
                                    #,@instance-var-methods
                                    #,@class-var-methods
                                    [else
                                     (let ([method (get-method 'name message #,@parent-ids)])
                                       (if (procedure? method)
                                           method
                                           #,default-method-code))]))
                            dispatch))]
                     [else (error (format "Bad message to class: ~a" message))])))))]))


;; Other functions that should be defined and exported, mostly copied
;; from obj.scm
(define (ask object message . args)
  (if (or (class? object) (object? object))
      (let ((method (object message)))
	(if (procedure? method)
	    (apply method args)
	    (error (format "No method ~a in class ~a" message method))))
      (error (format "Not an object: ~a" object))))

(define (dispatch-predicate fn message)
  (with-handlers ([exn:fail? (lambda (v) #f)])
    (and (procedure? fn)
         (equal? #t (fn message)))))

(define (object? x)
  (dispatch-predicate x 'object?))

(define (class? x)
  (dispatch-predicate x 'class?))

(define (instantiate class . arguments)
  (let ((new-instance (apply (class 'instantiate) arguments)))
    (ask new-instance 'initialize new-instance)
    new-instance))

(define (instantiate-parent class . arguments)
  (apply (class 'instantiate) arguments))

(define (get-method give-up-name message . objects)
  (if (null? objects)
      give-up-name
      (let ((method ((car objects) message)))
	(if (procedure? method)
	    method
	    (apply get-method give-up-name message (cdr objects))))))

(define-syntax-rule (usual arg ...)
  (ask dispatch 'send-usual-to-parent arg ...))
