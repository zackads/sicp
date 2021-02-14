#lang racket/base

(require berkeley)

;; Scheme calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((word? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + 0 (cdr args))))))
	((eq? fn '*) (accumulate * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate * 1 (cdr args))))))
        ((eq? fn 'first) (cond ((null? args) (error "Calc: no args to first"))
                               ((= (length args) 1) (first (first args)))
                               (else (first args))))
        ((eq? fn 'butfirst) (cond ((null? args) (error "Calc: no args to butfirst"))
                               ((= (length args) 1) (butfirst (first args)))
                               (else (butfirst args))))
        ((eq? fn 'last) (cond ((null? args) (error "Calc: no args to last"))
                               ((= (length args) 1) (last (first args)))
                               (else (last args))))
        ((eq? fn 'butlast) (cond ((null? args) (error "Calc: no args to butlast"))
                               ((= (length args) 1) (butlast (first args)))
                               (else (butlast args))))
        ((eq? fn 'word) (accumulate word (last args) (butlast args)))
	(else (error "Calc: bad operator:" fn))))
