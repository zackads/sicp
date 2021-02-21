#lang racket/gui

(require berkeley)

(define (equ? x y)
  (apply-generic 'equ? '(x y)))

; In install-scheme-number-package
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (equal? x y)))

; In install-rational-package
(define (equ-rat? x y)
  (and (equal? (numer x) (numer y))
       (equal? (denom x) (denom y))))
(put 'equ? '(rational rational) equ-rat?)

; In install-complex-package
(define (equ-com? x y)
  (and (equal? (real-part x) (real-part y))
       (equal? (imag-part x) (imag-part y))))
               