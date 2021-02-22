;; STEP 3: Global variables & define

(define (input-loop)
  (display "=61A=> ")
  (flush)
  
  (let ((input (read)))
    (if (equal? input 'exit)
	(print "Au Revoir!")
	(begin
	  (print (mc-eval input))
          (input-loop)))))


(define (mc-eval exp)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp))
	((if-exp? exp)
	 (if (not (eq? (mc-eval (cadr exp)) 'nay))
	     (mc-eval (caddr exp))
	     (mc-eval (cadddr exp))))
	((definition? exp) (define-variable!
			     (cadr exp)
			     (mc-eval (caddr exp))))
	((list? exp) (mc-apply (mc-eval (car exp))
			      (map mc-eval (cdr exp))))
	(else (error "UNKNOWN expression"))))



(define (mc-apply fn args)
  (do-magic fn args))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Related ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-variable! var val)
  (define (scan vars vals)
    (cond ((null? vars)
	   (set-car! the-global (cons var (car the-global)))
	   (set-cdr! the-global (cons val (cdr the-global))))
	  ((eq? var (car vars))
	   (set-car! vals val))
	  (else
	   (scan (cdr vars) (cdr vals)))))
  (scan (car the-global) (cdr the-global))
  var)

(define the-global
  (cons (list '+ '- '/ '* 'car 'cdr 'cons 'null? 'nil 'yell 'square 'factorial)
	(list + - / * car cdr cons null? nil yell square factorial)))

(define (lookup-variable-value var)
  (define (scan vars vals)
    (cond ((null? vars) (error "Unbound Variable"))
	  ((eq? var (car vars)) (car vals))
	  (else (scan (cdr vars) (cdr vals)))))
  (scan (car the-global)
	(cdr the-global)))

(input-loop)
