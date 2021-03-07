#lang racket

(require berkeley)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; Midterm 2, problem 2a (tree recursion)

(define (all-smaller? binary-tree n)
  (cond
    ((null? binary-tree) #t)
    ((> (entry binary-tree) n) #f)
    (else (and
           (all-smaller? (left-branch binary-tree) n)
           (all-smaller? (right-branch binary-tree) n)))))

(define (all-larger? binary-tree n)
  (cond
    ((null? binary-tree) #t)
    ((< (entry binary-tree) n) #f)
    (else (and
           (all-smaller? (left-branch binary-tree) n)
           (all-smaller? (right-branch binary-tree) n)))))

(define my-tree (make-tree 8 (make-tree 5 '() '())
                           (make-tree 12 '() '())))

(all-smaller? my-tree 15) ; #t
(all-smaller? my-tree 10) ; #f

; Midterm 2, problem 2b (tree recursion)
(define (bst? binary-tree)
  (or (null? binary-tree)
      (and (all-smaller? (left-branch binary-tree) (entry binary-tree))
           (all-larger? (right-branch binary-tree) (entry binary-tree))
           (bst? (left-branch binary-tree))
           (bst? (right-branch binary-tree))) ))

