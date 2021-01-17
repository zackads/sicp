;;; Trees, version 2
;;;
;;; Data at leaves only, leaves are words

(define (make-tree d ch)
  (cond ((null? d) ch)
	((null? ch) d)
	(else (error "Data at leaves only")) ))

(define (datum node)
  (cond ((null? node) '())
	((not (pair? node)) node)
	(else '()) ))

(define (children node)
  (cond ((null? node) '())
	((not (pair? node)) '())
	(else node) ))

(define (leaf? node)
  (not (pair? node)))

(define (treemap fn tree)
  (cond ((null? tree) '())
	((leaf? tree) (fn tree))
	(else (map (lambda (t) (treemap fn t)) tree)) ))

;; Sample

(define (square x) (* x x))

(define t2 '((1 2) 3 (4 (5 6))))

(define t3 (make-tree '()
		      (list (make-tree '() (list (make-tree 1 '())
						 (make-tree 2 '())))
			    (make-tree 3 '())
			    (make-tree '() (list (make-tree 4 '())
						 (make-tree '() (list (make-tree 5 '())
								      (make-tree 6 '()))))))))
							    	
