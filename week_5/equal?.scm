#lang racket/base

(require berkeley)

(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b)) (eq? a b))
        ((or (symbol? a) (symbol? b)) #f)
        ((and (number? a) (number? b)) (= a b))       
        ((or (number? a) (number? b)) #f)             
        ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))

(set! equal? equal?)
(trace equal?)

(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f