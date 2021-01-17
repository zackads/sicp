;; Tree ADT.
;;
;; Representation: a tree is a pair, whose car is the datum and whose
;; cdr is a list of the subtrees.

(define make-tree cons)
(define datum car)
(define children cdr)
(define empty-tree? null?)
(define (leaf? tree)
  (null? (children tree)))

;; Example tree, using ADT, with data at nodes.

(define t1 (make-tree 6
		      (list (make-tree 2
				       (list (make-tree 1 '())
					     (make-tree 4 '())))
			    (make-tree 9
				       (list (make-tree 7 '())
					     (make-tree 12 '()))))))

;; review -- mapping over a sequence.

(define (SQUARES seq)
  (if (null? seq)
      '()
      (cons (SQUARE (car seq))
	    (SQUARES (cdr seq)) )))

;; Mapping over a tree -- data at all nodes

(define (SQUARES tree)
  (make-tree (SQUARE (datum tree))
	     (map SQUARES (children tree)) ))

;; mapping over tree -- data at leaves only

(define (SQUARES tree)
  (cond ((empty-tree? tree) '())
	((leaf? tree) (make-tree (SQUARE (datum tree)) '()))
	(else (make-tree '() (map SQUARES (children tree)))) ))

;; Common alternative for mapping data at leaves only, no explicit ADT:

(define (SQUARES tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (SQUARE tree))
	(else (cons (SQUARES (car tree))
		    (SQUARES (cdr tree)) )) ))

;; Hallmark of tree recursion: recur for both car and cdr.
