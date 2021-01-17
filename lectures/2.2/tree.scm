;;; Implementation of tree ADT
;;;
;;; Data at leaves only; leaves are words.

(define (make-tree datum children)
  (cond ((null? datum) children)
	((not (null? children)) (error "datum at branch node!"))
	((list? datum) (error "datum must be a word!"))
	(else datum)))

(define (datum node)
  (if (list? node)
      '()
      node))

(define (children node)
  (if (list? node)
      node
      '() ))

(define (leaf? node)
  (not (pair? node)))

;; or can do:
;; (define (leaf? node)
;;  (null? (children node)) )

;; Map a function over the data of a tree

(define (treemap fn tree)
  (cond ((null? tree) '())
	((leaf? tree) (fn tree))
	(else (make-tree '() (map
			      (lambda (t) (treemap fn t))
			      (children tree))) )))

;; Sample

(define (square x) (* x x))

(define t1 '((1 2) 3 (4 (5 6))))

(treemap square t1)
