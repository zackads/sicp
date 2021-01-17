;; Printing the data in a binary tree.

;; ADT for tree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

;; sample trees.

(define tree1
  (make-tree 2
	     (make-tree 1 '() '())
	     (make-tree 3 '() '())))

(define tree2
  (make-tree 7
	     (make-tree 3
			(make-tree 1 '() '())
			(make-tree 5 '() '()))
	     (make-tree 9
			'()
			(make-tree 11 '() '()))))

;; Three printing procedures: pre-order, in-order, and post-order.

(define (pre-order tree)
  (cond ((null? tree) '())
	(else (print (entry tree))
	      (pre-order (left-branch tree))
	      (pre-order (right-branch tree)) )))

(define (in-order tree)
  (cond ((null? tree) '())
	(else (pre-order (left-branch tree))
	      (print (entry tree))
	      (pre-order (right-branch tree)) )))

(define (post-order tree)
  (cond ((null? tree) '())
	(else (pre-order (right-branch tree))
	      (pre-order (left-branch tree))
	      (print (entry tree)) )))

