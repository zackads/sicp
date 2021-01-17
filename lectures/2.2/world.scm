(define datum car)
(define children cdr)
(define make-tree cons)
(define (city x) (make-tree x '()))
(define (leaf? x) (null? (children x)))

(define world
  (make-tree
   'earth
   (list (make-tree
	  'united-states
	  (list (make-tree
		 'california
		 (list (city 'berkeley)
		       (city 'san-francisco)
		       (city 'palo-alto)
		       (city 'oakland)))
		(make-tree
		 'massachusetts
		 (list (city 'cambridge)
		       (city 'amherst)
		       (city 'boston)
		       (city 'sudbury)))
		(make-tree
		 'new-york
		 (list (city 'new-york)))))
	 (make-tree
	  'france
	  (list (city 'paris)
		(city 'lyon)
		(city 'marseilles))))))

(define (find-place place tree)
  (cond ((eq? place (datum tree)) (cons (datum tree) '()))
	((leaf? tree) '())
	(else (let ((try (find-subtree place (children tree))))
		(if (not (null? try))
		    (cons (datum tree) try)
		    '())))))

(define (find-subtree place nodes)
  (if (null? nodes)
      '()
      (let ((try (find-place place (car nodes))))
	(if (not (null? try))
	    try
	    (find-subtree place (cdr nodes))))))



