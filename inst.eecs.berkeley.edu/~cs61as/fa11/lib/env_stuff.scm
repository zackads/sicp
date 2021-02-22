;;; Allison Coates' notes:
;;; CS61A
;;; State, local state variables, environments

;;; Written 17 July 2001
;;; Moral rights owned by A Coates.

;;; Permission to use notes for research,
;;; academic and personal is permitted ONLY with the above citation
;;; pf my work and moral rights. No fee may be charged...



;;; motivation:

;;; the elements needed to implement OOP:
;;; local state, methods, assignment/mutation, inheritance
;;; we need a new way to do that:
;;; turns out, we'll be able to do it using 
;;; everything from before, and one additional primitive:
;;; set!

;;; BUT we need a new way of THINKING abuot what's going on.

;;; but more that that
(define (roots a b c)
  (define (roots1 d) (/ (+ (- b ) d) (* 2 a)))
  (roots1 (sqrt (- (* b b) (* 4 a c)))))


(define roots2 
  (lambda (a b c)
    (define (roots2-1 d) (/  (+ (- b) d) (* 2 a)))
    (roots2-1 (sqrt (- (* b b) (* 4 a c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; below we have 
(define (foo) (define bar 1))


;;; the above is really THIS:
;;; (define foo (lambda () (define bar 1))) 

;; this is NOT like:
;;; (define foo2 (define bar 3))
;; THIS IS VERY DIFFERENT.
;; how different?

(define foo (lambda () (define bar 1)))


bar 
;; not defined! 

foo
;; what does it return??!?!

bar

(foo)

bar

(define foo2 (define bar 3))

bar

foo2

bar

;;;;;

(define (add_me x)
  (define bar 1)
  (+ x bar))


;;; is really
(define add_me 
  (lambda (x)
    (define bar 1)
    (+ x bar)))


;;;(define add_me
;;;   (define bar 2 ) (...))
;;?
;; this won't work either: where's x defined???

;(define add_me2
;  (begin 
;    (define bar 2)
;    (+ x bar)))

(define add_me2
  (begin
    (define bar 2)
    (+ 5 bar)))


;; now ask
add_me2

bar

;;;; bar got changed "in the global environment"

(define add_me3
  (lambda (x)
    (define bar 3)
    (+ x bar)))


bar

(add_me3 10)

bar


;;;;;; so how do we explain these results?
;;;;; environments!

introduce the notion of a global environment.
introduce the rules for new environemnts:
for define and 
and for lambda expressions.
and for invocation of a lambda.



;;;;;;;;;;;;;;;;;;;; moving on to set!
;;;;;;;;;;;;;;;;;;;;


(define counter 0)

(define count 
  (lambda () 
    (set! counter (+ counter 1)) 
    counter))


;; counter

;; count

;; counter

;;(count)

;; (count)

;; counter


;;now, we don't want the counter in the global scope
;; we want to maintain local state that is updatable!

(define count2
  (begin 
    (define counter2 0)
    (lambda () 
      (set! counter2 (+ counter2 1))
      counter2)))



; (set! counter2 100)
; (count2)

;;nope, counter2 is STILL defined in the global scope



(define count3
  (let ((cntr3 0))
    (lambda ()
      (set! cntr3 (+ cntr3 1))
      cntr3)))