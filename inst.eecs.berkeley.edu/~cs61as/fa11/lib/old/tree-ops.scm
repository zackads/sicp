;; Support the 61A ADT Tree
;; -min

(define make-tree make-node)

(define (make-bt datum left right)
  (list datum left right))

(define datum car)

(define left-child cadr)

(define right-child caddr)

(define kennedy
  (make-tree 'Joseph
	     (list (make-tree 'John
			      (list (make-tree '(John Jr) nil)
				    (make-tree 'Caroline nil)))
		   (make-tree 'Robert
			      (list (make-tree 'Kathleen nil)))
		   (make-tree 'Edward nil))))

(define bush
  (make-tree '(George HW)
	     (list (make-tree '(George W)
			      (list (make-tree 'Jenna nil)
				    (make-tree 'Barbara nil)))
		   (make-tree 'Jeb
			      (list (make-tree 'Noelle nil)))
		   (make-tree 'Neil nil))))
