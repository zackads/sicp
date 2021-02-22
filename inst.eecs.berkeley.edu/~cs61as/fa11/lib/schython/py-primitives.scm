(define-class (py-obj)
  (method (true?) #f)
  (method (mutable?) #f)
  (method (none?) #f)
  (method (int?) #f)
  (method (float?) #f)
  (method (string?) #f)
  (method (bool?) #f)
  (method (list?) #f)
  (method (procedure?) #f)
  (method (primitive?) #f)
  (method (dictionary?) #f)
  (default-method
    (py-error "AttributeError: objects of type "
	      (objtype self)
	      " have no method " message)))

(define-class (none)
  (parent (py-obj))
  (method (none?) #t)
  (method (type) '*NONE*)
  (method (__str__) (make-py-string "None")))
(define *NONE* (instantiate none))

(define (make-py-string str)
  (instantiate py-string str))
(define-class (py-string val)
  (parent (py-obj))
  (method (true?) (not (zero? (string-length val))))
  (method (string?) #t)
  (method (type) 'py-string)
  (method (__str__) self)
  (method (__int__) (make-py-num (string->number val)))
  (method (__len__) (make-py-num (string-length val)))
  (method (__add__)
    (make-py-primitive
     '+
     (lambda (other)
       (cond ((ask other 'string?)
	      (make-py-string (string-append val (ask other 'val))))
	     (else (py-error "TypeError: Cannot concatenate string and "
			     (ask other 'type)))))))
  (method (__mul__)
    (make-py-primitive
     '*
     (lambda (other)
       (make-py-string
	(accumulate string-append
		    ""
		    (make-list (ask other 'val) val))))))
  (method (__getitem__ index)
    ;; does not handle three-index slices or inferred blanks
    (let ((n (ask (car index) 'val)))
      (if (not (integer? n))
	  (py-error "TypeError: string indices must be integers, not "
		    (objtype (car index))))
      (if (not (and (>= n (- (string-length val)))
		    (< n (string-length val))))
	  (py-error "IndexError: string index out of range: " n))
      (if (< n 0) (set! n (+ n (string-length val))))
      (if (null? (cdr index))
	  (make-py-string (substring val n (1+ n)))
	  (let ((m (ask (cadr index) 'val)))
	    (if (not (integer? m))
		(py-error "TypeError: string indices must be integers, not "
			  (objtype (cadr index))))
	    (if (not (and (>= m (- (string-length val)))
			  (< m (string-length val))))
		(py-error "IndexError: string index out of range: " m))
	    (if (< m 0) (set! m (+ m (string-length val))))
	    (if (not (null? (cddr index)))
		(py-error "only one- and two- element slices implemented"))
	    (make-py-string (substring val n m))))))
  (method (__reversed__)
    (make-py-primitive
     'reversed
     (lambda () (make-py-string (list->string (reverse (string->list val)))))))
  (method (__sorted__)
    (make-py-primitive
     'sorted
     (lambda ()
       (make-py-list (map (lambda (char) (make-py-string (string char)))
			  (sort (string->list val) char<?))))))
  (method (__iter__ var-name block env)
    (define (iter i)
      (if (>= i (string-length val))
	  *NONE*
	  (let ((result (eval-sequence
			 block
			 (begin (define-variable!
				  var-name
				  (make-py-string (substring val i (1+ i)))
				  env)
				env))))
	    (cond ((eq? result '*BREAK*) '*BREAK*)
		  ((and (pair? result) (eq? (car result) '*RETURN*)) result)
		  (else (iter (1+ i)))))))
    (let ((result (iter 0)))
      (if (and (pair? result) (eq? (car result) '*RETURN*))
	  result
	  *NONE*)))
  (method (capitalize)
    (make-py-primitve
     'capitalize
     (lambda ()
       (make-py-string (append-string (string (char-upcase (string-ref val 0)))
				      (substring val 1 (string-length val)))))))
  (method (__eq__)
    (make-py-primitive
     '==
     (lambda (other) (make-py-bool (string=? val (ask other 'val))))))
  (method (__ne__)
    (make-py-primitive
     '!=
     (lambda (other) (make-py-bool (not (string=? val (ask other 'val)))))))
  (method (__lt__)
    (make-py-primitive
     '<
     (lambda (other) (make-py-bool (string<? val (ask other 'val))))))
  (method (__gt__)
    (make-py-primitive
     '>
     (lambda (other) (make-py-bool (string>? val (ask other 'val))))))
  (method (__le__)
    (make-py-primitive
     '<=
     (lambda (other) (make-py-bool (string<=? val (ask other 'val))))))
  (method (__ge__)
    (make-py-primitive
     '>=
     (lambda (other) (make-py-bool (string>=? val (ask other 'val))))))
  (method (endswith)
    (make-py-primitive
     'endswith
     (lambda (other)
       (let ((len1 (string-length val))
	     (suffix (ask other 'val)))
	 (let ((len2 (string-length (ask other 'val))))
	   (if (> len2 len1)
	       (make-py-bool #f)
	       (let ((tail (substring val (- len1 len2) len1)))
		 (make-py-bool (string=? tail suffix)))))))))
  (method (isalnum)
    (make-py-primitive
     'isalnum
     (lambda ()
       (make-py-bool (accumulate (lambda (a b) (and a b))
				 #t
				 (map (lambda (char) (or (char-alphabetic? char)
							 (char-numeric? char)))
				      (string->list val)))))))
  (method (isalpha)
    (make-py-primitive
     'isalpha
     (lambda ()
       (make-py-bool (accumulate (lambda (a b) (and a b))
				 #t
				 (map char-alphabetic? (string->list val)))))))
  (method (isdigit)
    (make-py-primitive
     'isdigit
     (lambda ()
       (make-py-bool (accumulate (lambda (a b) (and a b))
				 #t
				 (map char-numeric? (string->list val)))))))
  (method (islower)
    (make-py-primitive
     'islower
     (lambda ()
       (make-py-bool (accumulate (lambda (a b) (and a b))
				 #t
				 (map char-lower-case? (string->list val)))))))
  (method (isspace)
    (make-py-primitive
     'isspace
     (lambda ()
       (make-py-bool (accumulate (lambda (a b) (and a b))
				 #t
				 (map char-whitespace? (string->list val)))))))
  (method (isupper)
    (make-py-primitive
     'isupper
     (lambda () (make-py-bool (accumulate (lambda (a b) (and a b))
					  #t
					  (map char-upper-case?
					       (string->list val)))))))
  (method (lower)
    (make-py-primitive
     'lower
     (lambda ()
       (make-py-string (list->string (map (lambda (char) (char-downcase char))
					  (string->list val)))))))
  (method (startswith)
    (make-py-primitive
     'startswith
     (lambda (other)
       (let ((len1 (string-length val))
	     (suffix (ask other 'val)))
	 (let ((len2 (string-length (ask other 'val))))
	   (if (> len2 len1)
	       (make-py-bool #f)
	       (let ((tail (substring val 0 len2)))
		 (make-py-bool (string=? tail suffix)))))))))
  (method (upper)
    (make-py-primitive
     'upper
     (lambda ()
       (make-py-string (list->string (map (lambda (char) (char-upcase char))
					  (string->list val)))))))
  )

(define (make-py-num num)
  (if (exact? num)
      (instantiate py-int num)
      (instantiate py-float num)))

(define-class (py-num val)
  ;; Parent class for py-int and py-float.  Never constructed, only exists to
  ;; provide wrappers for Scheme number functions to avoid duplicate code.
  (parent (py-obj))
  (method (true?) (not (zero? val)))
  (method (__str__) (make-py-string (number->string val)))
  (method (__add__)
    (make-py-primitive
     '+
     (lambda (other) (make-py-num (+ val (ask other 'val))))))
  (method (__sub__)
    (make-py-primitive
     '-
     (lambda (other) (make-py-num (- val (ask other 'val))))))
  (method (__mul__)
    (make-py-primitive
     '*
     (lambda (other) (make-py-num (* val (ask other 'val))))))
  (method (__div__)
    (make-py-primitive
     '/
     (lambda (other) (make-py-num (/ val (ask other 'val))))))
  (method (__pow__)
    (make-py-primitive
     '**
     (lambda (other) (make-py-num (expt val (ask other 'val))))))
  (method (__eq__)
    (make-py-primitive
     '==
     (lambda (other) (make-py-bool (= val (ask other 'val))))))
  (method (__ne__)
    (make-py-primitive
     '!=
     (lambda (other) (make-py-bool (not (= val (ask other 'val)))))))
  (method (__gt__)
    (make-py-primitive
     '>
     (lambda (other) (make-py-bool (> val (ask other 'val))))))
  (method (__lt__)
    (make-py-primitive
     '<
     (lambda (other) (make-py-bool (< val (ask other 'val))))))
  (method (__ge__)
    (make-py-primitive
     '>=
     (lambda (other) (make-py-bool (>= val (ask other 'val))))))
  (method (__le__)
    (make-py-primitive
     '<=
     (lambda (other) (make-py-bool (<= val (ask other 'val))))))
  )

(define-class (py-int val)
  (parent (py-num val))
  (method (int?) #t)
  (method (type) 'py-int)
  (method (__int__) self)
  (method (__float__)
    (instantiate py-float (exact->inexact val)))
  (method (__div__)
    (make-py-primitive
     '/
     (lambda (other)
       (if (ask other 'int?)
	   (make-py-num (quotient val (ask other 'val)))
	   (make-py-num (/ val (ask other 'val)))))))
  (method (__mod__)
    (make-py-primitive
     '%
     (lambda (other)
       (make-py-num (modulo val (ask other 'val))))))
  (method (__trunc__) self)
  )

(define-class (py-float val)
  (parent (py-num val))
  (method (float?) #t)
  (method (type) 'py-float)
  (method (__int__) (make-py-num (truncate val)))
  (method (__float__) self)
  (method (__mod__)
    (make-py-primitive
     '%
     (lambda (other) (make-py-num (modulo val (ask other 'val))))))
  (method (__trunc__) (make-py-num (truncate val)))
  )


(define-class (py-bool val)
  (parent (py-obj))
  (method (bool?) #t)
  (method (type) 'py-bool)
  (method (true?) val)
  (method (__eq__)
    (make-py-primitive
     '==
     (lambda (other) (make-py-bool (and (eq? (ask other 'type) 'py-bool)
					(eq? val (ask other 'val)))))))
  (method (__str__)
    (make-py-string (if val "True" "False"))))

(define *PY-TRUE* (instantiate py-bool #t))
(define *PY-FALSE* (instantiate py-bool #f))
(define (make-py-bool val) (if (memq val '(|True| #t)) *PY-TRUE* *PY-FALSE*))
(define (negate-bool bool) (py-error "TodoError: Person B, Question 4"))

(define (make-py-list val)
  (instantiate py-list val))
(define-class (py-list val)
  (parent (py-obj))
  (method (type) 'py-list)
  (method (list?) #t)
  (method (mutable?) #t)
  (method (true?) (not (zero? (length val))))
  (method (__str__)
    (make-py-string
     (string-append "["
		    (accumulate
		     (lambda (left right)
		       (if (equal? right "]")
			   (string-append left right)
			   (string-append left ", " right)))
		     "]"
		     (map (lambda (item)
			    (if (eq? (ask item 'type) 'py-string)
				(string-append (string #\")
					       (ask item 'val)
					       (string #\"))
				(ask (ask item '__str__) 'val)))
			  val)))))
  (method (__len__) (make-py-num (length val)))
  (method (reverse)
    (make-py-primitive 'reverse
		       (lambda () (set! val (reverse val)) *NONE*)))
  (method (__reversed__)
    (make-py-primitive 'reversed (lambda () (make-py-list (reverse val)))))
  (method (sort)
    (make-py-primitive
     'sort
     (lambda ()
       (set! val (sort val (lambda (a b) (ask (py-apply (ask a '__lt__)
							(list b))
					      'true?))))
       *NONE*)))
  (method (__sorted__)
    (make-py-primitive
     'sorted
     (lambda () (make-py-list (sort val (lambda (a b)
					  (ask (py-apply (ask a '__lt__)
							 (list b))
					       'true?)))))))
  (method (__iter__ var-name block env)
    (define (iter seq)
      (if (null? seq)
	  *NONE*
	  (begin (define-variable! var-name (car seq) env)
		 (let ((result (eval-sequence block env)))
		   (cond ((eq? result '*BREAK*) '*BREAK*)
			 ((and (pair? result) (eq? (car result) '*RETURN*))
			  result)
			 (else (iter (cdr seq))))))))
    (let ((result (iter val)))
      (cond ((and (pair? result) (eq? (car result) '*RETURN*)) result)
	    ((eq? result '*BREAK*) result)
	    (else *NONE*))))
  (method (__getitem__ index)
    (define (sublist seq start end)
      (cond ((> start 0) (sublist (cdr seq) (- start 1) end))
	    ((= end 0) '())
	    (else (cons (car seq) (sublist (cdr seq) start (- end 1))))))
    (let ((n (ask (car index) 'val)))
      (if (not (integer? n))
	  (py-error "TypeError: string indices must be integers, not "
		    (objtype (car index))))
      (if (not (and (>= n (- (length val))) (< n (length val))))
	  (py-error "IndexError: string index out of range: " n))
      (if (< n 0) (set! n (+ n (length val))))
      (if (null? (cdr index))
	  (list-ref val n)
	  (let ((m (ask (cadr index) 'val)))
	    (if (not (integer? m))
		(py-error "TypeError: string indices must be integers, not "
			  (objtype (cadr index))))
	    (if (not (and (>= m (- (length val))) (< m (length val))))
		(py-error "IndexError: string index out of range: " m))
	    (if (< m 0) (set! m (+ m (length val))))
	    (if (not (null? (cddr index)))
		(py-error "only one- and two- element slices implemented"))
	    (make-py-list (sublist val n m))))))
  (method (__setitem__ index item)
    (if (not (null? (cdr index))) (py-error "Slice-assignment not implemented"))
    (set! index (car index))
    (define (replace-item i seq)
      (cond ((null? seq)
	     (py-error "IndexError: list assignment out of range: "
		       (ask index 'val)))
	    ((zero? i) (set-car! seq item) *NONE*)
	    (else (replace-item (- i 1) (cdr seq)))))
    (let ((n (ask index 'val)))
      (if (not (integer? n)) (py-error "TypeError: list indices must be integers"))
      (if (not (and (>= n (- (length val))) (< n (length val))))
	  (py-error "IndexError: list index out of range: " n))
      (if (< n 0) (set! n (+ n (length val))))
      (replace-item n val)))
  (method (__contains__ other)
    (py-error "TodoError: Person A, Question 4"))
  (method (append)
    (make-py-primitive 'append
		       (lambda (item)
             (if (null? val)
                 (set! val (list item))
                 (append! val (list item)))
             *NONE*)))
  (method (insert) ;; only setup to add to the front, for efficiency
    (make-py-primitive 'insert
		       (lambda (item) (set! val (cons item val)) *NONE*)))
  (method (popleft)
    (make-py-primitive
     'popfront
     (lambda ()
       (if (null? val)
	   (py-error "IndexError: pop from empty list")
	   (let ((head (car val)))
	     (set! val (cdr val))
	     head)))))
  )
;;
;;table datatype. python dictionaries use this implementaiton of tables
(define (table-make keys values)
  (define (iter keys values)
    (if (null? keys)
      '()
      (cons (table-make-entry (car keys) (car values))
            (iter (cdr keys) (cdr values)))))
  (if (not (= (length keys) (length values)))
    (py-error "Dictionary error: Not same number of keys as values in dictionary")
    (iter keys values)))
(define (table-make-entry key val)
  (cons key val))
(define (table-first-entry table)
  (car table))
(define (table-first-entry-key table)
  (car (table-first-entry table)))
;;return the entry (a pair) or #f if not in table
(define (table-contains-key? table key compare-proc)
  (cond
    ((null? table)
     #f)
    ((compare-proc (table-first-entry-key table) key)
     (table-first-entry table))
    (else (table-contains-key? (cdr table) key compare-proc))))
(define (table-add-key-val-pair table key val compare-proc)
  (cond
    ((null? table)
      (cons (table-make-entry key val) table))
    ((compare-proc (table-first-entry-key table) key)
      (cons (table-make-entry key val) (cdr table)))
    (else (cons (car table)
                (table-add-key-val-pair (cdr table) key val compare-proc)))))
(define (table-for-each table proc)
  (for-each (lambda (n)
              (proc (car n) (cdr n)))
            table))
(define (table-get-keys t)
  (map car t))
(define (table-get-vals t)
  (map cdr t))

(define (assert-or-error pred description)
  (if (not pred)
    (py-error description)))

(define (make-py-dictionary pairs)
  (instantiate py-dictionary pairs))

(define-class (py-dictionary table)
  (parent (py-obj))
  (instance-vars (comp-proc (lambda (x y)
                             (and (eq? (ask x 'type) (ask y 'type))
                                  (eq? (py-apply (ask x '__eq__) (list y)) *PY-TRUE*)))))
  (initialize
    (for-each (lambda (x) (ask self 'isValidKey (car x)))
              table))
  (method (type) 'py-dictionary)
  (method (dictionary?) #t)
  (method (mutable?) #t)
  (method (true?) (not (null? table)))
  (method (isValidKey key)
    (assert-or-error (not (ask key 'mutable?)) "Dictionary Error: All keys to the dictionary must be IMMUTABLE objects"))
  (method (__str__)
    (make-py-string
         (string-append "{\n"
            (accumulate (lambda (left right)
                          (if (equal? right "}")
                            (string-append "  " left "\n" right)
                            (string-append "  " left ",\n" right)))
                        "}"
                        (map (lambda (item)
                               (let ((key (car item))
                                     (val (cdr item)))
                                 (if (eq? (ask key 'type) 'py-string)
                                   (set! key (string-append (string #\") (ask key 'val) (string #\")))
                                   (set! key (string-append (ask (ask key '__str__) 'val))))
                                 (if (eq? (ask val 'type) 'py-string)
                                   (set! val (string-append (string #\") (ask val 'val) (string #\")))
                                   (set! val (string-append (ask (ask val '__str__) 'val))))
                                 (string-append key " : " val )))
                             table)))))
  (method (__setitem__ py-obj-key py-obj-val)
    (ask self 'isValidKey py-obj-key)
    (set! table (table-add-key-val-pair table py-obj-key py-obj-val comp-proc))
    *NONE*)
  (method (__getitem__ key)
    (let ((v (table-contains-key? table key comp-proc)))
      (if v
        (cdr v) ;return the value
        (py-error (string-append "Dictionary doesn't contain the key: " (ask (ask key '__str__) 'val))))))
  (method (__contains__ key)
    (let ((v (table-contains-key? table key comp-proc)))
      (if v
        (make-py-bool #t)
        (make-py-bool #f))))
  (method (__keys__)
    (make-py-list (table-get-keys table)))
  (method (__vals__)
    (make-py-list (table-get-vals table)))
  )

(define (make-py-proc name params body env)
  (instantiate py-proc name params body env))
(define-class (py-proc name params body env)
  (parent (py-obj))
  (instance-vars (num-params #f))
  (initialize (set! num-params (length params)))
  (method (type) 'py-proc)
  (method (procedure?) #t)
  (method (__str__)
    (make-py-string
     (apply string-append (list "<function " (symbol->string name) ">"))))
  (method (__call__ args)
    (define (execute block env)
      (if (null? block)
	  *NONE*
	  (let ((line-obj (make-line-obj (car block))))
	    (if (and (not (empty? line-obj)) ;; check for tail call
		     (eq? (ask line-obj 'peek) 'return))
		(begin (ask line-obj 'next) ;; discard return token
		       (py-eval line-obj env))
		(let ((val (py-eval line-obj env)))
		  (if (and (pair? val) (eq? (car val) '*RETURN*))
		      (cdr val)
		      (execute (cdr block) env)))))))
    (let ((num-args (length args)))
      (cond ((> num-args num-params)
	     (py-error "TypeError: Too many arguments to " name))
	    ((< num-args num-params)
	     (py-error "TypeError: Too few arguments to " name))
	    (else (execute body (extend-environment params args env))))))
  )

(define (make-py-primitive name proc)
  (instantiate py-primitive name proc))
(define-class (py-primitive name proc)
  (parent (py-obj))
  (method (type) 'py-proc)
  (method (primitive?) #t)
  (method (__str__)
    (make-py-string (apply string-append
			   (list "<method " (symbol->string name) ">"))))
  (method (__call__ args) (apply proc args))
  )

(define (square x) (* x x)) ;; math helper
(define-class (math)
  (parent (py-obj))
  (method (__str__) (make-py-string "<built-in module 'math'>"))
  ;; Mathematical constants
  (class-vars (pi (make-py-num (* 4 (atan 1))))
	      (e (make-py-num (exp 1)))
	      (phi (make-py-num (/ (+ 1 (sqrt 5)) 2))))
  ;; Number-theoretic functions
  (method (ceil)
    (make-py-primitive 'ceil
		       (lambda (num) (make-py-num (ceiling (ask num 'val))))))
  (method (fabs)
    (make-py-primitive 'fabs
		       (lambda (num)
			 (let ((val (ask num 'val)))
			   (if (< val 0)
			       (make-py-num (- val))
			       num)))))
  (method (factorial)
    (make-py-primitive
     'factorial
     (lambda (num)
       (define (fact-iter n p)
	 (if (= n 0)
	     p
	     (fact-iter (- n 1) (* p n))))
       (if (or (not (ask num 'int?))
	       (< (ask num 'val) 0))
	   (py-error "ValueError: factorial() not defined for negative values")
	   (fact-iter (ask num 'val) 1)))))
  (method (floor)
    (make-py-primitive 'floor
		       (lambda (num) (make-py-num (floor (ask num 'val))))))
  (method (trunc)
    (make-py-primitive 'trunc
		       (lambda (num) (ask num '__trunc__))))
  ;; Power and logarithmic functions
  (method (exp)
    (make-py-primitive 'exp
		       (lambda (num) (make-py-num (exp (ask num 'val))))))
  (method (log)
    (make-py-primitive
     'log
     (lambda (num . base)
       (if (null? base)
	   (make-py-num (log (ask num 'val)))
	   (make-py-num (/ (log (ask num 'val))
			   (log (ask (car base) 'val))))))))
  (method (log10)
    (make-py-primitive 'log10
		       (lambda (num)
			 (make-py-num (/ (log (ask num 'val)) (log 10))))))
  (method (pow)
    (make-py-primitive 'pow
		       (lambda (x y)
			 (make-py-num (expt (ask x 'val) (ask y 'val))))))
  (method (sqrt)
    (make-py-primitive 'sqrt
		       (lambda (num) (make-py-num (sqrt (ask num 'val))))))
  ;; Trigonometric functions
  (method (acos)
    (make-py-primitive 'acos
		       (lambda (num) (make-py-num (acos (ask num 'val))))))
  (method (asin)
    (make-py-primitive 'asin
		       (lambda (num) (make-py-num (asin (ask num 'val))))))
  (method (atan)
    (make-py-primitive 'atan
		       (lambda (num) (make-py-num (atan (ask num 'val))))))
  (method (atan2)
    (make-py-primitive
     'atan2
     (lambda (x y) (make-py-num (atan (ask x 'val) (ask y 'val))))))
  (method (cos)
    (make-py-primitive
     'cos
     (lambda (num) (make-py-num (cos (ask num 'val))))))
  (method (hypot)
    (make-py-primitive
     'hypot
     (lambda (x y) (make-py-num (sqrt (+ (square (ask x 'val))
					 (square (ask y 'val))))))))
  (method (sin)
    (make-py-primitive
     'sin
     (lambda (num) (make-py-num (sin (ask num 'val))))))
  (method (tan)
    (make-py-primitive
     'tan
     (lambda (num) (make-py-num (tan (ask num 'val))))))
  ;; Angular conversion functions
  (method (degrees)
    (make-py-primitive
     'degrees
     (lambda (num) (make-py-num (* 180 (/ (ask num 'val) pi))))))
  (method (radians)
    (make-py-primitive
     'radians
     (lambda (num) (make-py-num (* pi (/ (ask num 'val) 180))))))
  ;; Hyperbolic functions:
  (method (asinh)
    (make-py-primitive
     'asinh
     (lambda (num)
       (make-py-num (log (+ (ask num 'val)
			    (sqrt (1+ (square (ask num 'val))))))))))
  (method (acosh)
    (make-py-primitive
     'acosh
     (lambda (num)
       (make-py-num (log (+ (ask num 'val)
			    (sqrt (- (square (ask num 'val)) 1))))))))
  (method (atanh)
    (make-py-primitive
     'atanh
     (lambda (num)
       (make-py-num (* .5 (log (/ (+ 1 (ask num 'val))
				  (- 1 (ask num 'val)))))))))
  (method (sinh)
    (make-py-primitive 'sinh
		       (lambda (num)
			 (make-py-num
			  (* .5 (- (exp (ask num 'val))
				   (exp (- (ask num 'val)))))))))
  (method (cosh)
    (make-py-primitive 'cosh
		       (lambda (num)
			 (make-py-num (* .5 (+ (exp (ask num 'val))
					       (exp (- (ask num 'val)))))))))
  (method (tanh)
    (make-py-primitive 'tanh
		       (lambda (num)
			 (make-py-num
			  (/ (- (exp (* 2 (ask num 'val))) 1)
			     (+ (exp (* 2 (ask num 'val))) 1))))))
  )

(define-class (py-random)
  (parent (py-obj))
  (method (__str__) (make-py-string "<built-in module 'random'>"))
  (method (randrange)
    (make-py-primitive
     'randrange
     (lambda args
       (cond ((null? args)
	      (py-error "TypeError: Too few arguments to randrange"))
	     ((null? (cdr args)) (make-py-num (random (ask (car args) 'val))))
	     ((null? (cddr args))
	      (let ((start (ask (car args) 'val))
		    (end (-1+ (ask (cdr args 'val)))))
		(make-py-num (+ start (random (- end start))))))
	     ((null? (cdddr args))
	      (let ((start (ask (car args) 'val))
		    (end (ask (cadr args) 'val))
		    (step (ask (caddr args) 'val)))
		(set! end (- end (quotient (- end start) step)))
		(make-py-num (+ start (* step (random (- end start)))))))
	     (else (py-error "TypeError: Too many arguments to randrange"))))))
  (method (randint)
    (make-py-primitive
     'randint
     (lambda (a b)
       (make-py-num (+ (ask a 'val) (random (1+ (ask b 'val))))))))
  (method (choice)
    (make-py-primitive
     'choice
     (lambda (seq)
       (let ((len (ask (ask seq '__len__) 'val)))
	 (ask seq '__getitem__ (make-py-num (random len)))))))
  (method (random)
    (make-py-primitive 'random
		       (lambda () (make-py-num (/ (random 4000000000))))))
  )


(define (define-primitives!)
  (define (add-prim name proc)
    (define-variable! name (make-py-primitive name proc) the-global-environment))
  (define-variable! 'math (instantiate math) the-global-environment)
  (define-variable! 'random (instantiate py-random) the-global-environment)
  (add-prim 'abs
	    (lambda (num)
	      (if (< (ask num 'val) 0) (make-py-num (- (ask num 'val))) val)))
  (add-prim 'bin
	    (lambda (int)
	      (let ((n (ask int 'val)))
		(let ((str (number->string n 2)))
		  (make-py-string
		   (if (< n 0)
		       (string-append "-0b"
				      (substring str 1 (string-length str)))
		       (string-append "0b" str)))))))
  (add-prim 'bool (lambda arg (if (null? arg)
				  (make-py-bool #f)
				  (make-py-bool (ask (car arg) 'true?)))))
  (add-prim 'chr (lambda (num)
		   (make-py-string (string (integer->char (ask num 'val))))))
  (add-prim 'cmp (lambda (x y)
		   (cond ((py-apply (ask x '__lt__) (list y)) -1)
			 ((py-apply (ask x '__gt__) (list y)) 1)
			 (else 0))))
  (add-prim 'divmod
	    (lambda (a b)
	      (make-py-list (list (py-apply (ask a '__div__) (list b))
				  (py-apply (ask b '__mod__) (list b))))))
  (add-prim 'float (lambda (num) (ask num '__float__)))
  (add-prim 'hex
            (lambda (int)
              (let ((n (ask int 'val)))
                (let ((str (number->string n 16)))
                  (make-py-string
                   (if (< n 0)
                       (string-append "-0x"
                                      (substring str 1 (string-length str)))
                       (string-append "0x" str)))))))
  (add-prim 'int (lambda (num) (ask num '__int__)))
  (add-prim 'len (lambda (seq) (ask seq '__len__)))
  (add-prim 'oct
	    (lambda (int)
              (let ((n (ask int 'val)))
                (let ((str (number->string n 8)))
                  (make-py-string
                   (if (< n 0)
                       (string-append "-0"
                                      (substring str 1 (string-length str)))
                       (string-append "0" str)))))))
  (add-prim
   'ord
   (lambda (char)
     (if (not (= (ask (ask char '__len__) 'val) 1))
	 (py-error "TypeError: Expected string of length 1")
	 (make-py-num (char->integer (string-ref (ask char 'val) 0))))))
  (add-prim
   'pow
   (lambda (base pow . mod)
     (define (mexpt b n m)
       (cond ((= n 0) 1)
             ((even? n) (modulo (mexpt (modulo (* b b) m) (/ n 2) m) m))
             (else (modulo (* b (modulo (mexpt (modulo (* b b) m)
                                               (quotient n 2) m)
                                        m))
                           m))))
     (if (null? mod)
         (py-apply (ask base '__pow__) (list pow))
         (make-py-num (mexpt (ask base 'val)
                             (ask pow 'val)
                             (ask (car mod) 'val))))))
  (add-prim
   'range
   (lambda (num . other-args)
     (define (make-range low cur step so-far)
       (if (< cur low)
           (make-py-list so-far)
           (make-range low (- cur step) step (cons (make-py-num cur) so-far))))
     (cond ((null? other-args) (make-range 0 (-1+ (ask num 'val)) 1 '()))
           ((null? (cdr other-args))
            (make-range (ask num 'val) (-1+ (ask (car other-args) 'val)) 1 '()))
           (else
            (let ((start (ask num 'val))
                  (end (ask (car other-args) 'val))
                  (step (ask (cadr other-args) 'val)))
              (cond ((= step 0)
                     (py-error "ValueError: range() step argument cannot be zero"))
                    ((> step 0)
		     (let ((last-value (- end (modulo (- end start) step))))
		       (make-range start last-value step '())))
                    (else
                     (let ((result (make-range (1+ end) start (- step) '())))
		       (py-apply (ask result 'reverse) '())
                       result))))))))
  (add-prim 'raw_input
	    (lambda arg
	      (define (read-line so-far)
		(let ((char (read-char)))
		  (if (or (eof-object? char)
			  (eq? char #\newline))
		      so-far
		      (read-line (string-append so-far (string char))))))
	      (if (not (null? arg))
		  (begin (display (ask (ask (car arg) '__str__) 'val)) (flush)))
	      (make-py-string (read-line ""))))
  (add-prim 'reversed
	    (lambda (obj) (ask (ask obj '__reversed__) '__call__ '())))
  (add-prim 'sorted
	    (lambda (obj) (ask (ask obj '__sorted__) '__call__ '())))
  (add-prim 'str (lambda (obj) (ask obj '__str__)))
  (add-prim 'type (lambda (obj) (make-py-type (objtype obj))))
  )

(define (make-py-type type)
  (instantiate py-type type))
(define-class (py-type val)
  (parent (py-obj))
  (method (__str__) (make-py-string val)))

(define (objtype obj)
  (cdr (assq (ask obj 'type)
	     '((*NONE* . "<type 'NoneType'>")
	       (py-int . "<type 'int'>")
	       (py-float . "<type 'float'>")
	       (py-bool . "<type 'bool'>")
	       (py-list . "<type 'list'>")
         (py-dictionary . "<type 'dictionary'>")
	       (py-string . "<type 'str'>")
	       (py-proc . "<type 'function'>")))))

(define (py-list? val) (equal? (ask val 'type) 'py-list))
(define (py-dict? val) (equal? (ask val 'type) 'py-dictionary))
