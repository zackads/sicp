#lang racket/base

(require berkeley)

(define (square n)
  (* n n))

; Using map
(define (square-tree-map tree)
  (if (number? tree)
      (* tree tree)
      (map square-tree-map tree)))

; Without higher-order-functions
(define (square-tree tree)
  (cond ((null? tree) '())
        ((number? tree) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))