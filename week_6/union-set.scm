#lang racket

(require berkeley)

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2) (cons x1 (union-set set1 (cdr set2)))))))))

(define ordered-set-1 (list 1 2 3))
(define ordered-set-2 (list 2 4 6))
(union-set ordered-set-1 ordered-set-2) ; (1 2 3 4 6)

(define ordered-set-3 (list 1 2 3))
(define ordered-set-4 (list 2 4 6 8 10 12))
(union-set ordered-set-3 ordered-set-4) ; (1 2 3 4 6 8 10 12)