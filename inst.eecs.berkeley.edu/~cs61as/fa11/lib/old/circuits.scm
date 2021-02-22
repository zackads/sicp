(require "obj")

;;; Utilities

;; True iff V is a proper signal value (0 or 1).
(define (signal? v) (memv v '(0 1)))

;; Does nothing if the expression VAL evaluates to true; 
;; otherwise causes an error.
(define-macro (assert assertion)
  `(if (not ,assertion)
       (error (format #f "Assertion fails: ~a~%" (uncode ',assertion)))))

;; Destructively remove all elements equal? to OBJ from LST,
;; returning the resulting list.
(define (remove! obj lst)
  (cond ((null? lst) lst)
	((equal? obj (car lst)) (remove! obj (cdr lst)))
	(else (set-cdr! lst (remove! obj (cdr lst)))
	      lst)))

;;; Gates

;; A GATE is an object that has NUM-INPUTS inputs, and NUM-OUTPUTS outputs.
;; Whenever one of its inputs is set via the set-input! method, it recomputes 
;; its outputs (available via the output method).
(define-class (gate num-inputs num-outputs)
  (instance-vars (outputs (make-list num-outputs 0))
		 (inputs (make-list num-inputs 0))
		 (observers '()))
  (method (output n) (assert (< -1 n num-outputs)) (nth n outputs))
  (method (set-input! n v) (assert (and (< -1 n num-inputs) (signal? v)))
	  (set-car! (nthcdr n inputs) v) (ask self 'compute)
	  (for-each (lambda (observer) (ask observer 'notice self))
		    observers))
  (method (register-observer whom)
	  (set! observers (cons whom observers)))
  (method (remove-observer whom)
	  (set! observers (remove! whom observers))))
	  

;;; Definitions of common gates.

(define-class (and-gate num-inputs)
  (parent (gate num-inputs 1))
  (method (compute)
    (set-car! (ask self 'outputs) 
       (apply * (ask self 'inputs)))))

(define-class (or-gate num-inputs)
  (parent (gate num-inputs 1))
  (method (compute)
    (set-car! (ask self 'outputs)
       (min 1 (apply + (ask self 'inputs))))))

(define-class (inverter)
  (parent (gate 1 1))
  (method (compute)
	  (set-car! (ask self 'outputs) (- 1 (car (ask self 'inputs))))))

;; A terminal simply transfers its single input to its single 
;; output.
(define-class (terminal)
  (parent (gate 1 1))
  (method (compute) 
     (set-car! (ask self 'outputs) (car (ask self 'inputs)))))

;;; Wires

;; A object that connects input of one gate to an output of another.
;; Initially, it is unconnected. The connect method establishes a 
;; connection, breaking any previously existing connection.
(define-class (wire)
  (instance-vars (from-gate #f) (from-port #f) (to-gate #f) (to-port #f))
  (method (connect gate-in input-number gate-out output-number)
    (if from-gate (ask from-gate 'remove-observer self))
    (set! to-gate gate-in) (set! from-gate gate-out)
    (set! to-port input-number) (set! from-port output-number)
    (ask gate-out 'register-observer self)
    (ask self 'transmit))
  (method (notice notifier)
    (ask self 'transmit))
  (method (transmit)
    (if (and to-gate from-gate)
	(ask to-gate 'set-input! to-port
	     (ask from-gate 'output from-port)))))

;; Convenience function: Create a wire connecting input #TO-NUM of 
;; gate TO to output #FROM-NUM of gate FROM, returning the wire.
;; FROM-NUM is an optional argument: it is 0 if OPT-FROM-NUM is null,
;; and otherwise the first element of OPT-FROM-NUM.
(define (attach to to-num from . opt-from-num)
   (let ((the-wire (instantiate wire))
	 (from-num (if (null? opt-from-num) 0 (car opt-from-num))))
      (ask the-wire 'connect to to-num from from-num)  the-wire))

