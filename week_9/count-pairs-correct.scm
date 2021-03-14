#lang racket

(define (count-pairs x)
  (define unique-pairs '())
  (if (or (not (pair? x)) (member x unique-pairs))
      0
      (begin
        (append! x unique-pairs)
        (print (member x unique-pairs))
        (+  (count-pairs (car x))
            (count-pairs (cdr x))
            1))))

(define three-pairs1 (cons 1 (cons 2 (cons 3 4))))

(define one-pair (cons 'a 'b))
(define three-pairs2 (cons (cons one-pair one-pair) 'a))

(define two-pairs (cons one-pair one-pair))
(define three-pairs3 (cons two-pairs two-pairs))

(count-pairs three-pairs1) ; 3
(count-pairs three-pairs2) ; 3
(count-pairs three-pairs3) ; 3