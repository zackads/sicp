#lang racket

(require berkeley)

(define make-tree cons)
(define datum car)
(define children cdr)

; Midterm 2, problem 3 (tree recursion)
(define (treemap fn tree)
  (make-tree (fn (datum tree))
             (forest-map fn (children tree))))

(define (forest-map fn forest)
  (if (null? forest)
      '()
      (cons (treemap fn (car forest))
            (forest-map fn (cdr forest)))))

(define (max-fanout tree)
  (apply max (treemap (lambda (tree) (length (children tree))) tree)))