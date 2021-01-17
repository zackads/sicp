(define (depth-first-search tree)
  (print (datum tree))
  (for-each depth-first-search (children tree)))

(define (breadth-first-search tree)
  (bfs-iter (list tree)))

(define (bfs-iter queue)
  (if (null? queue)
      'done
      (let ((task (car queue)))
	(print (datum task))
	(bfs-iter (append (cdr queue) (children task))))))
