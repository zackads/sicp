(define (make-collect-body result bindings restriction)
  (define (collect-let-bindings bindings n)
    (if (null? bindings) '()
	(cons
	 `(,(caar bindings)
	   (car ,((repeated (lambda (x) `(cdr ,x)) n) 'tuple)))
	 (collect-let-bindings (cdr bindings) (+ n 1)))))
  (define (collect-let-body rest-of-bindings)
    (if (null? (cdr rest-of-bindings))
	`(map (lambda (,(caar rest-of-bindings))
		(list ,@(map car bindings)))
	      ,(cadar rest-of-bindings))
	`(flatmap
	  (lambda (,(caar rest-of-bindings))
	    ,(collect-let-body (cdr rest-of-bindings)))
	  ,(cadar rest-of-bindings))))
  `(map (lambda (tuple)
	  (let ,(collect-let-bindings bindings 0)
	    ,result))
	,(if (null? restriction)
	     (collect-let-body bindings)
	     `(filter (lambda (tuple)
			(let ,(collect-let-bindings bindings 0)
			  ,(car restriction)))
		      ,(collect-let-body bindings)))))

(define-macro (collect result bindings . restriction)
	     (make-collect-body result bindings restriction))



(define (prime-sum-pairs n)
  (collect (list i j (+ i j))
	   ((i (enumerate-interval 1 n))
	    (j (enumerate-interval 1 (-1+ i))))
	   (prime? (+ i j))))

(define (prime? n)
  (define (iter factor)
    (cond ((= factor n) #t)
          ((= (remainder n factor) 0) #f)
          (else (iter (1+ factor)))))
  (iter 2))

