#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;; Tree stuff

(define make-tree make-node)
(define (leaf? node)
	(null? (children node)))
(define (treemap fn tree)
	(make-tree (fn (datum tree))
               (map (lambda (t) (treemap fn t)) (children tree))))

;; Binary Trees

(define make-binary-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)