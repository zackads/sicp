#lang racket/base

(require berkeley)

(define (shorter? a b)
  (< (count a) (count b)))

(define (in-order? pred? sent)
  (cond ((equal? (count sent) 1) #t)
        ((pred? (first sent) (first (butfirst sent))) (in-order? pred? (butfirst sent)))
        (else #f)))

; (in-order? shorter? '(i saw them standing together)) ;f
; (in-order? shorter? '(i saw her standing there)) ;f
; (in-order? < '(2 3 5 5 8 13)) ;f
; (in-order? <= '(2 3 5 5 8 13)) ;t
; (in-order? > '(23 14 7 5 2)) ;t

(define (order-checker pred?)
  (lambda (sent) (in-order? pred? sent)))

(define length-ordered? (order-checker shorter?))

(length-ordered? '(i saw them standing together)) ; #f
(length-ordered? '(i saw her standing there)) ; #f
((order-checker <) '(2 3 5 5 8 13)) ; #f
((order-checker <=) '(2 3 5 5 8 13)) ; #t
((order-checker >) '(23 14 7 5 2)) ; #t