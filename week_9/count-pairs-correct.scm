#lang racket

(require berkeley)

(define (count-pairs x)
  (define unique-pairs '())
  (define (count y)
    (if (or (not (pair? y)) (member y unique-pairs))
        0
        (begin
          (set! unique-pairs (append (list y) unique-pairs))
          (+  (count (car y))
              (count (cdr y))
              1))))
  (count x))

(define three-pairs1 (cons 1 (cons 2 (cons 3 4))))

(define one-pair (cons 'a 'b))
(define three-pairs2 (cons (cons one-pair one-pair) 'a)) ; 3 unique pairs

(define two-pairs (cons one-pair one-pair))
(define three-pairs3 (cons two-pairs two-pairs)) ; 3 unique pairs

(count-pairs three-pairs1) ; 3
(count-pairs three-pairs2) ; 3
(count-pairs three-pairs3) ; 3