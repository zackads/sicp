(define make-square list)

(define row car)

(define column cadr)

(define adas-maze
  (list
   (make-square 1 3) (make-square 1 4)
   (make-square 2 1) (make-square 2 2) (make-square 2 4)
   (make-square 3 1) (make-square 3 4) (make-square 3 5)
   (make-square 4 1) (make-square 4 2) (make-square 4 4)
   (make-square 5 2) (make-square 5 3) (make-square 5 4)))

(define (neighbors square)
  (list (make-square (- (row square) 1) (column square))
	(make-square (+ (row square) 1) (column square))
	(make-square (row square) (- (column square) 1))
	(make-square (row square) (+ (column square) 1))))

(define (find-path start goal maze)
  (try-paths (list (list start)) goal maze))

(define (try-paths paths goal maze)
  (define (try-loop p)
    (cond ((null? p)
	   (try-paths (extend-all-paths paths maze)
		      goal
		      maze))
	  ((complete? goal (car p))
	   (car p))
	  (else (try-loop (cdr p)))))
  (try-loop paths))

(define (complete? goal path)
  (same-square? goal (car path)))

(define (same-square? s1 s2)
  (and (= (row s1) (row s2))
       (= (column s1) (column s2))))

(define (extend-all-paths paths maze)
  (if (null? paths)
      '()
      (append (extend-path (car paths)
			   (neighbors (car (car paths)))
			   maze)
	      (extend-all-paths (cdr paths) maze))))

(define (extend-path path neighbors maze)
  (cond ((null? neighbors) '())
  	((allowed-extension? (car neighbors) maze)
   	 (cons (cons (car neighbors) path)
	       (extend-path path (cdr neighbors) maze)))
  	(else (extend-path path (cdr neighbors) maze))))

(define (allowed-extension? square maze)
  (square-in-list? square maze))

(define (square-in-list? square lst)
  (cond ((null? lst) #f)
	((same-square? square (car lst)) #t)
	(else (square-in-list? square (cdr lst)))))

