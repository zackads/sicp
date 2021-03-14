#lang racket

(define (count-pairs x)
    (if (not (pair? x))
        0
        (+  (count-pairs (car x))
            (count-pairs (cdr x))
            1)))

(define three-pairs1 (cons 1 (cons 2 (cons 3 4))))

(define one-pair (cons 'a 'b))
(define three-pairs2 (cons (cons one-pair one-pair) 'a))

(define two-pairs (cons one-pair one-pair))
(define three-pairs3 (cons two-pairs two-pairs))

(define infinite-loop null) ; ???

(count-pairs three-pairs1) ; 3
(count-pairs three-pairs2) ; 4
(count-pairs three-pairs3) ; 7
(count-pairs infinite-loop) ; Didn't get this one, apparently needed to use make-cycle