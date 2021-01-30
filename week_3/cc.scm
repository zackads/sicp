#lang racket/base

(require berkeley)

(define (count-change1 amount) (cc1 amount 5))
(define (count-change2 amount) (cc2 amount 5))

(define (cc1 amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc1 amount
                     (- kinds-of-coins 1))
                 (cc1 (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (cc2 amount kinds-of-coins)
  (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
        ((= amount 0) 1)
        (else (+ (cc2 amount
                     (- kinds-of-coins 1))
                 (cc2 (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))