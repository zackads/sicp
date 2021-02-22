(define (make-branch q y n)
  (list 'branch q y n))

(define (make-leaf a)
  (list 'leaf a))

(define (type node) (car node))			; all nodes
(define (leaf? node) (eq? (type node) 'leaf))
(define (branch? node) (eq? (type node) 'branch))

(define (answer node) (cadr node))		; leaf nodes

(define (question node) (cadr node))		; branch nodes
(define (yespart node) (caddr node))
(define (nopart node) (cadddr node))

(define (set-yes! node x)
  (set-car! (cddr node) x))
(define (set-no! node x)
  (set-car! (cdddr node) x))


(define animal-tree
  (make-leaf 'rabbit))

(define (animal-game)
  (print "Think of an animal.  I'll guess it.")
  (animal animal-tree (lambda (x) (set! animal-tree x))))


(define (animal node setter)
  (if (branch? node)
      (if (yorn (question node))
	  (animal (yespart node) (lambda (new) (set-yes! node new)))
	  (animal (nopart node) (lambda (new) (set-no! node new))))
      (if (yorn (word "Is it " (a/an (answer node)) "?"))
	  "I win!!!"
	  (begin
	   (display "I give up, what is it? ")
	   (flush)
	   (let ((correct (read)))
	     (newline)
	     (display "Please tell me a question whose ")
	     (display "answer is YES for ")
	     (display (a/an correct))
	     (newline)
	     (display "and NO for ")
	     (display (a/an (answer node)))
	     (display ".")
	     (newline)
	     (display "Enclose the question in quotation marks.")
	     (newline)
	     (let ((newquest (read)))
	       (setter (make-branch
			newquest
			(make-leaf correct)
			node))
	       "Thanks.  Now I know better."))))))

(define (yorn question)
  (display question)
  (display " ")
  (flush)
  (let ((yn (read)))
    (cond ((eq? (first yn) 'y) #t)
	  ((eq? (first yn) 'n) #f)
	  (else (print "Please type YES or NO.")
		(yorn question)))))

(define (a/an wd)
  (if (member? (first wd) 'aeiou)
      (word "an " wd)
      (word "a " wd)))
