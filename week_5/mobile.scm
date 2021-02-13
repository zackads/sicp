#lang racket/base

(require berkeley)

; Constructors
;(define (old-make-mobile left right) (list left right))
;(define (old-make-branch length structure) (list length structure))
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))

; Selectors
; (define (old-right-branch mobile) (cadr mobile))
; (define (old-branch-structure branch) (cadr branch))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

; Utilities
(define (branch-weight branch)
  (if (number? (branch-structure branch))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced-branch? branch)
  (if (number? (branch-structure branch))
      #t
      (balanced? (branch-structure branch))))
           
(define (balanced? mobile)
  (and (equal? (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
       (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))
  
; Tests

(balanced? (make-mobile
            (make-branch 2 (make-mobile (make-branch 2 6) (make-branch 4 4)))
            (make-branch 2 12))) ;f