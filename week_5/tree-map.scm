#lang racket/base

(require berkeley)

(define (square n)
  (* n n))

(define (tree-map fn tree)
  (if (number? tree)
      (fn tree)
      (map (lambda (t) (tree-map fn t)) tree)))

(define (square-tree tree)
  (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))