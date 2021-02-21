#lang racket/gui

(require berkeley)

(define (=zero? n)
  (apply-generic '=zero? n))

; In install-scheme-number-package
(put '=zero? 'scheme-number zero?)

; In install-rational-package
(define (=zero-rat? n)
  (equ? n (make-rat 0)))
; ...
(put '=zero? 'rational =zero-rat?)

; In install-complex-package
(define (=zero-com? n)
  (equ? n (make-complex 0)))
; ...
(put '=zero? 'complex =zero-com?)