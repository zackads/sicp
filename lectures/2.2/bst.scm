;;; bst.scm -- Binary Search Trees (A & S section 2.2.5)
;;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define tree1
    (make-tree 7
	(make-tree 3
	    (make-tree 1 '() '())
	    (make-tree 5 '() '()))
	(make-tree 9 '() (make-tree 11 '() '()))))

(define (element-of-set? x set)
    (cond
	((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	    (element-of-set? x (left-branch set)))
	((> x (entry set))
	    (element-of-set? x (right-branch set)))))
