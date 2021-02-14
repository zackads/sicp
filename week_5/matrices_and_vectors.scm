#lang racket/base

(require berkeley)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (matrix-row) (dot-product v matrix-row)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

; (transpose (list (list 7 8 9) (list 9 10 11)))
; (matrix-*-vector (list (list 7 8 9) (list 9 10 11)) (list 12 13 14))

(set! matrix-*-vector matrix-*-vector)
(set! dot-product dot-product)
(trace matrix-*-vector dot-product)

(matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 7 8) (list 9 10) (list 11 12)))