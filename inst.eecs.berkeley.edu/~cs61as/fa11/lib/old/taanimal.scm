(define (animal node)
  (define (type node) (car node))
  (define (question node) (cadr node))
  (define (yespart node) (caddr node))
  (define (nopart node) (cadddr node))
  (define (answer node) (cadr node))
  (define (leaf? node) (eq? (type node) 'leaf))
  (define (branch? node) (eq? (type node) 'branch))
  (define (set-yes! node x)
    (set-car! (cddr node) x))
  (define (set-no! node x)
    (set-car! (cdddr node) x))

  (define (yorn)
    (let ((yn (read)))
      (cond ((eq? yn 'yes) #t)
	    ((eq? yn 'no) #f)
	    (else (display "Please type YES or NO!  >")
		  (yorn)))))

  (display (question node))
  (display " ")
  (let ((yn (yorn)))
    (let ((next (if yn (yespart node) (nopart node))))
      (cond ((branch? next) (animal next))
	    (else (display "Is it ")
		  (display (answer next))
		  (display "? ")
		  (let ((correctflag (yorn)))
		    (cond (correctflag "I win!")
			  (else (newline)
				(display "I give up, what is it? ")
				(let ((correct (read)))
				  (newline)
				  (display "Please tell me a question whose ")
				  (display "answer is YES for ")
				  (display correct)
				  (newline)
				  (display "and NO for ")
				  (display (answer next))
				  (display ".")
				  (newline)
				  (display "Enclose the question in ")
				  (display "quotation marks.")
				  (newline)
				  (let ((newquest (read)))
				    (if yn
					(set-yes! node
						  (make-branch
						    newquest
						    (make-leaf correct)
						    next))
					(set-no! node
						 (make-branch
						   newquest
						   (make-leaf correct)
						   next)))
				    "Thanks.  Now I know better."))))))))))

(define (make-branch q y n)
  (list 'branch q y n))

(define (make-leaf a)
  (list 'leaf a))

(define animal-list
  (make-branch "Is it a Lab Assistant?"
	       (make-branch "Is it female?"
			    (make-leaf 'Dawn)
			    (make-leaf 'Jay))
	       (make-leaf 'Kurt)))


(define (animal-game) (animal animal-list))
