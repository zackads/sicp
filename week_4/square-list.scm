#lang racket/base

(require berkeley)

(define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))

(define (square-list items)
  (iter items nil))

(set! square-list square-list)
(set! iter iter)

(trace square-list iter)

