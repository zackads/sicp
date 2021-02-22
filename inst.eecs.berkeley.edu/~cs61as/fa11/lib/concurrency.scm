;; Implementation of parallel-execute using call/cc.
;;
;; By Ben Rudiak-Gould, 10/2002.
;;
;; Requires STk (for "procedure-body" and first-class environments).


(define call/cc call-with-current-continuation)


(define (parallel-execute . thunks)
  (apply run-concurrently-with-env
         random
         (map (lambda (thunk)
                (cons (list (uncode (procedure-body thunk)))
		      (make-virtual-env (procedure-environment thunk)) ))
              thunks ))
  'okay )


(define (run-concurrently select . exprs)
  (apply run-concurrently-with-env
	 select
	 (map (lambda (x)
		(cons x (make-virtual-env (global-environment))) )
	      exprs )))


(define (run-concurrently-with-env select . exprs-with-envs)
  (let ((threads
	 (map (lambda (exp-env)
		(list (call/cc
		       (lambda (cont)
			 (let ((scheduler (call/cc cont)))
			   (scheduler (myeval (car exp-env)
					      (cdr exp-env)
					      scheduler )))))))
	      exprs-with-envs )))
    (let loop ()
      (let ((active-threads
             (filter (lambda (x) (continuation? (car x))) threads) ))
        (if (null? active-threads)
            (map car threads)
            (let ((active (list-ref active-threads
                                    (select (length active-threads)) )))
              (set-car! active (call/cc (car active)))
              (loop) ))))))


(define (make-virtual-env real-env)
  (cons
   `((quote    **macro** ,macro-quote)
     (lambda   **macro** ,macro-lambda)
     (let      **macro** ,macro-let)
     (set!     **macro** ,macro-set!)
     (define   **macro** ,macro-define)
     (if       **macro** ,macro-if)
     (cond     **macro** ,macro-cond)
     (and      **macro** ,macro-and)
     (or       **macro** ,macro-or)
     (set-car! **prim**  ,prim-set-car!)
     (set-cdr! **prim**  ,prim-set-cdr!)
     (begin    **prim**  ,prim-begin)
     (test-and-set! **prim** ,prim-test-and-set!) )
   real-env ))


(define (env-lookup-raw sym env scheduler)
  (call/cc scheduler)
  (let ((virtual (assq sym (car env))))
    (if virtual
        (cdr virtual)
        (eval sym (cdr env)) )))


(define (env-lookup sym env scheduler)
  (let* ((val (env-lookup-raw sym env scheduler))
         (proc-body (procedure-body val)) )
    (if (and proc-body (not (eq? (cadr proc-body) '**args**)))
        (myeval (uncode proc-body)
                (make-virtual-env (procedure-environment val))
                scheduler )
        val )))


(define (env-set! sym val env scheduler)
  (call/cc scheduler)
  (let ((virtual (assq sym (car env))))
    (if virtual
        (set-cdr! virtual val)
        (eval `(set! ,sym ',val) (cdr env)) )))


(define (env-define! sym val env scheduler)
  (call/cc scheduler)
  (set-car! env (cons (cons sym val) (car env))) )


(define (get-special-form name env scheduler)
  (if (symbol? name)
      (let ((val (env-lookup-raw name env scheduler)))
        (if (and (pair? val) (eq? (car val) '**macro**))
            val
            #f ))
      #f ))


(define (myeval expr env scheduler)
  (cond ((pair? expr)
         (let ((special (get-special-form (car expr) env scheduler)))
           (if special
               ((cadr special) (cdr expr) env scheduler)
               (let ((evaluated (eval-seq expr env scheduler)))
                 (myapply (car evaluated) (cdr evaluated) scheduler) ))))
        ((symbol? expr)
	 (env-lookup expr env scheduler) )
        (else (eval expr)) ))


(define (eval-seq exprs env scheduler)
  (if (null? exprs)
      '()
      (let ((val (myeval (car exprs) env scheduler)))
	(cons val (eval-seq (cdr exprs) env scheduler)) )))


(define (myapply func args scheduler)
  (cond ((procedure? func)
         (apply func args) )
        ((and (pair? func) (eq? (car func) '**prim**))
         ((cadr func) args scheduler) )
        ((and (pair? func) (eq? (car func) '**macro**))
         ((cadr func) (map (lambda (x) (list 'quote x)) args) scheduler) )
        (else (error "apply of non-procedure" func args)) ))


(define (make-call-environment params args env)
  (cons (let loop ((params params) (args args))
          (cond ((pair? params)
                 (cons (cons (car params) (car args))
                       (loop (cdr params) (cdr args)) ))
                ((null? params)
                 (car env) )
                (else (cons (cons params args) (car env))) ))
        (cdr env) ))


(define (macro-lambda args env scheduler)
  (let ((params (car args))
        (body (cdr args)) )
    (lambda **args**
      (let ((new-env (make-call-environment params **args** env)))
        (last (map (lambda (x) (myeval x new-env scheduler)) body)) ))))


(define (macro-let args env scheduler)
  (let ((vars (map car (car args)))
        (vals (map cadr (car args)))
        (body (cdr args)) )
    (myeval `((lambda ,vars ,@body) ,@vals) env scheduler) ))


(define (macro-define args env scheduler)
  (if (pair? (car args))
      (macro-define `(,(caar args) (lambda ,(cdar args) ,@(cdr args)))
		    env scheduler )
      (let ((val (myeval (cadr args) env scheduler)))
        (env-define! (car args) val env scheduler) )))


(define (macro-set! args env scheduler)
  (let ((val (myeval (cadr args) env scheduler)))
    (env-set! (car args) val env scheduler) ))


(define (macro-quote args env scheduler)
  (car args) )


(define (macro-if args env scheduler)
  (if (myeval (car args) env scheduler)
      (myeval (cadr args) env scheduler)
      (if (pair? (cddr args))
	  (myeval (caddr args) env scheduler)
	  'okay )))


(define (macro-cond args env scheduler)
  (cond ((null? args) 'okay)
        ((or (eq? (caar args) 'else)
             (myeval (caar args) env scheduler) )
         (car (last-pair (eval-seq (cdar args) env scheduler))) )
        (else (macro-cond (cdr args) env scheduler)) ))


(define (macro-and args env scheduler)
  (if (null? args)
      #t
      (let ((val (myeval (car args) env scheduler)))
        (if (null? (cdr args))
            val
            (and val (macro-and (cdr args) env scheduler)) ))))


(define (macro-or args env scheduler)
  (if (null? args)
      #f
      (let ((val (myeval (car args) env scheduler)))
        (if (null? (cdr args))
            val
            (or val (macro-or (cdr args) env scheduler)) ))))


(define (prim-set-car! args scheduler)
  (call/cc scheduler)
  (apply set-car! args) )


(define (prim-set-cdr! args scheduler)
  (call/cc scheduler)
  (apply set-cdr! args) )


(define (prim-begin args scheduler)
  (car (last-pair args)) )


(define (prim-test-and-set! args scheduler)
  (call/cc scheduler)
  (test-and-set! (car args)) )


(define (test-and-set! x)
  (let ((oldval (car x)))
    (set-car! x #t)
    oldval ))


(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst)) ))


(load "~cs61as/lib/serial.scm")
