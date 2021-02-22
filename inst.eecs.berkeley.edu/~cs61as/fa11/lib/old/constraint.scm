;; Programming with constraints, from section 3.3.5 of Abelson and Sussman.

;; Syntactic interface to contraint and probe objects.
;; These operations inform them that a value has become defined or undefined;
;; they have to figure out which value is involved.

(define (inform-about-value constraint)
  ((constraint 'I-have-a-value)))

(define (inform-about-no-value constraint)
  ((constraint 'I-lost-my-value)))

;; Types of constraints defined here: adder, multiplier and constant;
;; also define probe, which is a pseudo-constraint.

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   process-new-value)
	  ((eq? request 'I-lost-my-value)
	   process-forget-value)
	  (else
	   (error "Unknown request -- ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (if (has-value? m1) (= (get-value m1) 0) #f)
	       (if (has-value? m2) (= (get-value m2) 0) #f))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product
		       (* (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? m1) (has-value? product))
	   (set-value! m2
		       (/ (get-value product) (get-value m1))
		       me))
	  ((and (has-value? m2) (has-value? product))
	   (set-value! m1
		       (/ (get-value product) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   process-new-value)
	  ((eq? request 'I-lost-my-value)
	   process-forget-value)
	  (else
	   (error "Unknown request -- MULTIPLIER" request))))

  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (process-new-value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display (get-value connector))
    (newline))

  (define (process-forget-value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display "?")
    (newline))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   process-new-value)
	  ((eq? request 'I-lost-my-value)
	   process-forget-value)
	  (else
	   (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

;; syntactic interface to connector objects

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;; connector object generator.

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))

    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (error "Contradiction" (list value newval)))))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin (set! informant #f)
		 (for-each-except retractor
				  inform-about-no-value
				  constraints))))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	  (set! constraints
		(cons new-constraint constraints)))
      (if (has-value? me)
	  (inform-about-value new-constraint)))

    (define (me request)
      (cond ((eq? request 'has-value?)
	     (not (null? informant)))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown operation -- CONNECTOR" request))))
    me))

;; Helper procedure for connector:
;; do the procedure for each element of list EXCEPT the exception.
;; used to inform connected constraints of value changes
;; (don't want to inform the source of the change)

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) exception) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))

;; Example application: Centigrade/Fahrenheit converter.
;; Internal connectors and constraints are stashed in a private environment.
;; see Ex. 3.37 for a cleaner way to do this.

(define (centigrade-fahrenheit-converter c f)
  (let ((u (make-connector))
       (v (make-connector))
       (w (make-connector))
       (x (make-connector))
       (y (make-connector)))

       (multiplier c w u)
       (multiplier v x u)
       (adder v y f)
       (constant 9 w)
       (constant 5 x)
       (constant 32 y)))

(define C (make-connector))
(define F (make-connector))
(centigrade-fahrenheit-converter C F)
(probe "centigrade temp" C)
(probe "Fahrenheit temp" F)

(define (fma-constraint f m a)
  (multiplier m a f))
(define force (make-connector))
(define mass (make-connector))
(define acceleration (make-connector))
(fma-constraint force mass acceleration)
(probe "force" force)
(probe "mass" mass)
(probe "acceleration" acceleration)
