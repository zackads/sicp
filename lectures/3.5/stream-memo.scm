;; a non-memoized stream implementation....

(define newa (let((a 0))
	       (lambda()(set! a (+ 1 a)) a)))

(define x (cons-stream (newa) x))

(define (ystream)(cons-stream (newa) (ystream)))

(define y (ystream))

(show-stream y)

;; change def of force/stream-cdr/ delay  so it doesn't do memoization
;; this procedure->memo stuff
;; is how one defines "new" special forms in our scm.
;; Don't learn it..

(base:define delay (procedure->macro (lambda (x env)
				  `(lambda() ,(cadr x)))))
(define y (ystream))

;; used to be defined this way (see lib/berkeley.scm)
;;(define delay (procedure->macro (lambda (x env)
;;				  `(memo-proc (lambda() ,(cadr x))))))


;..................  if we disable memoization

> (define newa (let((a 0))
	       (lambda()(set! a (+ 1 a)) a)))
newa
> (define (ystream)(cons-stream (newa) (ystream)))
ystream
> (define y (ystream))
y
> (show-stream y)
(1 2 3 4 5 6 7 8 9 10 ...)
> (show-stream y)   ;; we recompute all but the first element, with (newa)
(1 12 13 14 15 16 17 18 19 20 ...)
> (define y (ystream))  ;; now we recompute ALL the elements, with (newa)
y
> (show-stream y)
(22 23 24 25 26 27 28 29 30 31 ...)

> (show-stream y)  ;; yes, we really saw that...
(22 33 34 35 36 37 38 39 40 41 ...)
> 
