;;;; AMB EVALUATOR FROM SECTION 4.3 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;; This is ambeval.scm, with several additions by Ka-Ping Yee to show
;;;; an above-the-line trace of what's happening in the evaluator.

;;;; To start the evaluator, run (mce) .


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.2 definition
;; of eval overrides the definition from 4.1.1
(load "~cs61a/lib/mceval.scm")

(load "~cs61a/lib/tables.scm")
(define *lambda-name-table* (make-table))
(define *procedure-name-table* (make-table))
(define *trace-indentation* 0)

(define (clean obj)
  (define (clean-obj obj seen)
      (if (pair? obj)
          (if (member obj seen) '...
              (cons (clean-obj (car obj) (cons obj seen))
                    (clean-obj (cdr obj) (cons obj seen))
              )
          )
          obj
      )
  )

  (clean-obj obj '())
)


;;;Code from SECTION 4.3.3, modified as needed to run it

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for Let
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (let ((result (make-procedure vars bproc env))
                     (name (lookup exp *lambda-name-table*)))
                 (if name (insert! result name *procedure-name-table*))
                 result
               )
               fail))))

;;;Conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;;;Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-named-lambda (caadr exp) (cdadr exp) (cddr exp))))

(define (make-named-lambda name parameters body)
  (let ((result (make-lambda parameters body)))
       (insert! result name *lambda-name-table*)
       result
  ))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

;;;Procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (define (succeed4 value fail4)
                               (show-return proc value)
                               (succeed value fail4)
                           )
                           (execute-application
                               proc args succeed4 fail3))
                         (lambda ()
                           (show-failure proc)
                           (fail2)
                         )
               ))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (show-invocation proc args)
    (let ((name (lookup proc *procedure-name-table*)))
         (if name
             (begin
                 (set! *trace-indentation* (+ *trace-indentation* 2))
                 (define (show-args formals actuals)
                    (display (car formals))
                    (display " = ")
                    (display (clean (car actuals)))
                    (if (not (null? (cdr formals)))
                        (begin
                            (display ", ")
                            (show-args (cdr formals) (cdr actuals))
                        )
                    )
                 )
                 (display (make-string *trace-indentation* #\.))
                 (display " -> ")
                 (display name)
                 (if (not (null? args))
                     (begin
                         (display " with ")
                         (show-args (procedure-parameters proc) args)
                     ))
                 (newline)
             )
         )
    )
)

(define (show-return proc result)
    (let ((name (lookup proc *procedure-name-table*)))
         (if name
             (begin
                 (display (make-string *trace-indentation* #\.))
                 (display " <- ")
                 (display name)
                 (display " succeeds returning ")
                 (display (clean result))
                 (newline)
                 (set! *trace-indentation* (- *trace-indentation* 2))
             )
         )
    )
)

(define (show-failure proc)
    (let ((name (lookup proc *procedure-name-table*)))
         (if name
             (begin
                 (display (make-string *trace-indentation* #\.))
                 (display " ")
                 (display name)
                 (display " fails!")
                 (newline)
             )
         )
    )
)

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         (show-invocation proc args)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail
         )
        )
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;;amb expressions

(define (show-select exp selected)
  (display (make-string *trace-indentation* #\.))
  (display " ")
  (display exp)
  (display " selects ")
  (display selected)
  (newline)
)

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices exps)
        (define saved-indentation *trace-indentation*)
        (if (null? choices)
            (fail)
            (begin
                (show-select exp (car exps))
                ((car choices) env
                               succeed
                               (lambda ()
                                 (set! *trace-indentation* saved-indentation)
                                 (try-next (cdr choices) (cdr exps)))))))
      (try-next cprocs (amb-choices exp)))))

;;;Driver loop

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (display ";;; Starting a new problem.")
            (set! *trace-indentation* 0)
            (newline)
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

;; modified by JDL (5-5-2000)

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
        (list '< <=)
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


;;; Added at Berkeley:

(define (mce)
  (set! the-global-environment (setup-environment))
  (ambeval '(define (require p) (if (not p) (amb)))
	   the-global-environment
	   (lambda (a b) #t)
	   (lambda () #t))
  (driver-loop))
