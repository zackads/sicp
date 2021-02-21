#lang racket/gui

(require berkeley)

; Constructors
(define (make-rational-from-integer n) (make-rat n 1))
(define (make-real-from-rational n) (make-real (numerator n) (denominator n)))

; Coercion procedures
(define (integer->rational n) (make-rational-from-integer (contents n)))
(define (rational->real n) (make-real-from-rational (contents n)))

; Install to coercion table
(put-coercion 'integer 'rational integer->ration)
(put-coercion 'rational 'real rational->real)

; Install to type-operation table of the rational and integer packages
(put 'raise '(integer) integer->rational)
(put 'raise '(rational) rational->complex)

(define (raise n)
  (apply-generic 'raise n))


