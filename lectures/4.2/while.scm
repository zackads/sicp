 (define (while a b)(if (a) (begin (force b) (while a b)) 'done))

 (define (while a b)(if (a) (begin (b) (while a b)) 'done))

(let ((x 10))
(while (lambda() (> x 0)) (lambda()(display x)(set! x (- x 1))))) ;works


(define (while a b)(if (a) (begin (b) (while a b)))


(let ((x 10))
  (while (delay (> x 0))
       (delay (begin (display x)
		     (set! x (- x 1))))
	  ; does not work because of memoization
	 ))
