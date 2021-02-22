;;; APL interpreter project      apl.scm

(define (----YOU-FILL-THIS-IN----) '())   ; just so file will load

;;; Step 1: convert scalar procedures to array procedures

(define (single x)     ; reduce order of single-element list
  (cond ((not (pair? x)) x)
	((null? (cdr x)) (single (car x)))
	(else x)))

(define (apl-dyadic op)    ; turn dyadic scalar function into APL array fn
  (define (newop x y)
    (let ((xx (single x))
	  (yy (single y)))
      (cond ((and (number? xx) (number? yy))
     	     (----YOU-FILL-THIS-IN----))
	    ((number? xx) (map (----YOU-FILL-THIS-IN----) yy))
	    ((number? yy) (map (----YOU-FILL-THIS-IN----) xx))
	    (else (map newop xx yy)))))
  newop)

;;; Step 2: APL primitive operations

(define (iota n)       ; monadic iota
  (define (iter x n)
    (if (> x n)
	'()
	(cons x (iter (1+ x) n))))
  (iter 1 (single n)))

(define (reshape shape l)  ; dyadic rho
  (define (circular l)
    (define (c1 pair)
      (if (null? (cdr pair))
	  (set-cdr! pair l)
	  (c1 (cdr pair))))
    (c1 l)
    l)
  (define token
    (let ((source (circular (ravel l))))
      (lambda ()
	(let ((out (car source)))
	  (set! source (cdr source))
	  out))))
  (define (string n shape)
    (if (= n 0)
	'()
	(let ((top (re1 shape)))
	  (cons top (string (-1+ n) shape)))))
  (define (re1 shape)
    (if (null? shape)
      	(token)
      	(string (car shape) (cdr shape))))
  (re1 shape))

(define (cat a b)       ; dyadic comma
  (define (depth l)
    (if (not (pair? l))
	0
	(1+ (depth (car l)))))
  (define (max x y)
    (if (> x y) x y))
  (define (shapeup l dims)
    (if (= dims (depth l))
	l
	(shapeup (cons l '()) dims)))
  (let ((dim (max (depth a) (depth b))))
    (append (shapeup a dim) (shapeup b dim))))

(define (ravel l)       ; monadic comma
  (define (r1 this rest)
    (cond ((null? this) (ravel rest))
	  ((not (pair? this)) (cons this (ravel rest)))
	  (else (r1 (car this) (cons (cdr this) rest)))))
  (cond ((null? l) '())
	((not (pair? l)) (cons l '()))
	(else (r1 (car l) (cdr l)))))

(define (abs x)        ; monadic bar
  (if (< x 0) (- x) x))

(define (rem x y)      ; dyadic bar
  (remainder y x))

;;; data abstraction for APL operators

(define (tri-op name nil mon dy)
  (list name nil mon dy))

(define (apl-operator? op)
  (assq op apl-operators))

(define (niladic op)
  (cadr (assq op apl-operators)))

(define (monadic op)
  (caddr (assq op apl-operators)))

(define (dyadic op)
  (cadddr (assq op apl-operators)))

(define (make-niladic name body)
  (set! apl-operators (cons (tri-op name body error error) apl-operators)))

(define (make-monadic name body)
  (set! apl-operators (cons (tri-op name error body error) apl-operators)))

(define (make-dyadic name body)
  (set! apl-operators (cons (tri-op name error error body) apl-operators)))

(define (make-op op mon dy)   ; abbreviation for primitives, never niladic
  (tri-op op error mon dy))

(define (make-scalar-op op mon dy)
  (make-op op (apl-monadic mon) (apl-dyadic dy)))

;;; Table of operations

(define (apl-pred2 op)   ; turn Lisp predicate (t/f) into APL (0/1)
  (lambda (x y)
    (if (op x y) 1 0)))

(define apl-operators
  (list (make-scalar-op '+ (lambda (x) x) +)
	(make-scalar-op '- - -)
	(make-scalar-op '*
			(lambda (x) (cond ((< x 0) -1)
					  ((= x 0) 0)
					  (else 1)))
			*)
	(make-scalar-op '% / /)
	(make-scalar-op 'bar abs rem)
	(make-scalar-op '= error (apl-pred2 =))
	(make-scalar-op '< error (apl-pred2 <))
	(make-scalar-op '> error (apl-pred2 >))
	(make-op '/ error compress)
	(make-op 'iota iota error)
	(make-op 'rho shape reshape)
	(make-op 'comma ravel cat)))

;;; APL higher-order operations

(define (reduce op l)   ; higher-order /
  (if (null? (cdr l))
      (car l)
      ((dyadic op) (car l) (reduce op (cdr l)))))

(define (apl-hof? op)
  (assq op apl-hofs))

(define (hof op)
  (cdr (assq op apl-hofs)))

(define apl-hofs (list (cons '/ reduce)))

;;; Step 3: Syntax conversion, infix to prefix

(define (get-operand l)
  (cond ((null? l) '())
	((list? (car l))
	 (get-dyad (get-operand (car l)) (cdr l)))
	((number? (car l)) (get-vector l))
	((apl-operator? (car l))
	 (cond ((null? (cdr l)) (error "dangling operator" (car l)))
	       ((apl-hof? (cadr l))
		(list (hof (cadr l)) (car l) (get-operand (cddr l))))
	       (else
	 	(list (monadic (car l)) (get-operand (cdr l))))))
	(else (get-dyad (car l) (cdr l)))))

(define (get-dyad left l)
  (cond ((null? l) left)
	((apl-operator? (car l))
	 (----YOU PUT SOMETHING HERE----))
	(else
	 (error "operand where operator expected" (car l)))))

(define (get-vector l)
  (define (gv vect l)
    (cond ((null? l) vect)
	  ((number? (car l))
	   (----YOU PUT SOMETHING HERE----))
	  (else (get-dyad vect l))))
  (gv '() l))

;;; mini-evaluator

(define (apl-loop)
  (define (maybe-display val)
    (if (eq? val 'no-value) '() (display val)))
  (newline)
  (display "APL> ")
  (maybe-display (apl-eval (get-operand (read))))
  (apl-loop))

(define (apl-eval l)
  (cond ((not (pair? l)) l)
	((procedure? (car l)) (apply (car l) (map apl-eval (cdr l))))
	(else l)))

(define *keyboard-interrupt-handler* reset)

;;; Step 11: Procedure definition

(define (convert-syntax l)
  (cond ((not (pair? l)) l)
	((eq? (car l) 'del) (proc-definition (cdr l)) 'no-name)
	(else (get-operand l))))

(define (proc-definition l)
  (cond ((null? (cdr l))
	 (make-niladic (car l) (make-procedure '() '() '() (proc-body))))
	((eq? (cadr l) 'gets)
	 (proc-result (car l) (cddr l)))
	(else (----YOU-FILL-THIS-IN----))))

(define (proc-result outvar l)
  (define (count-to-locals l)
    (cond ((null? l) 0)
	  ((eq? (car l) ':) 0)
	  (else (1+ (count-to-locals (cdr l))))))
  (define (locals l)
    (cond ((null? l) '())
	  ((eq? (car l) ':)
	   (cons (cadr l) (locals (cddr l))))
	  (else (error "bad format in locals" l))))
  (let ((adic (count-to-locals l)))
    (cond ((= adic 1)
	   (make-niladic (car l)
			 (make-procedure outvar
					 '()
					 (locals (cdr l))
					 (proc-body))))
	  ((= adic 2)
	   (make-monadic (car l)
			 (make-procedure outvar
					 (list (cadr l))
					 (locals (cddr l))
					 (proc-body))))
	  ((= adic 3)
	   (----YOU-FILL-THIS-IN----))
	  (else (error "too many args in function definition" l)))))

(define (proc-body)
  (define (proc-body-loop lineno)
    (display "[")
    (display lineno)
    (display "]  ")
    (let ((next (read)))
      (if (eq? next 'del)
	  '()
	  (cons next (proc-body-loop (1+ lineno))))))
  (proc-body-loop 1))
