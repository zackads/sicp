#lang racket

;;;; This library contains all additions needed to make
;;;; Simply Scheme and SICP work smoothly with Racket.
;;;;
;;;; Maintainer: Allen Guo <allenguo@berkeley.edu>

; XREPL (extensions for Racket REPL)

(require xrepl)

; Mutable list support

(require compatibility/mlist)

; Simply Scheme support

(require (planet dyoo/simply-scheme:2:2))

; Enable make-tree for trees

(define make-tree make-node)

; SICP support
; From http://planet.racket-lang.org/display.ss?package=sicp.plt&owner=neil

(define true #t)
(define false #f)
(define nil '())

(define (identity x) x)
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define the-empty-stream '())
(define (stream-null? x) (null? x))

; Stream procedures
; From berkeley.scm

(define (stream-car stream) (car stream))
(define (stream-cdr st)
  (force (cdr st)))

(define (berkeley:stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (berkeley:stream-ref (stream-cdr s) (- n 1))))

(define (berkeley:stream-map proc . s)
  (if (stream-null? (car s))
      the-empty-stream
      (cons-stream (apply proc (map stream-car s))
                   (apply berkeley:stream-map proc (map stream-cdr s)))))

(define (berkeley:stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
       (proc (stream-car s))
       (berkeley:stream-for-each proc (stream-cdr s)))))

(define (berkeley:stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
  ((pred (stream-car stream))
   (cons-stream (stream-car stream)
          (berkeley:stream-filter pred (stream-cdr stream))))
  (else (berkeley:stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (berkeley:stream-for-each displayln s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (scale-stream strm factor)
  (berkeley:stream-map (lambda (x) (* x factor)) strm))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (list->stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (list->stream (cdr l)))))

(define (show-stream strm . args)
  (if (null? args)
      (ss1 strm 10 10)
      (ss1 strm (car args) (car args))))

(define ss show-stream)

(define (ss1 strm this all)
  (cond ((null? strm) '())
  ((= this 0) '(...))
  ((and (pair? strm) (promise? (cdr strm)))
   (cons (ss1 (stream-car strm) all all)
         (ss1 (stream-cdr strm) (- this 1) all)))
  (else strm)))

; Helpful stream generators (from Lesson 10)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define ones (cons-stream 1 ones))

; Other helpful procedures from various lessons

(define (square x) (* x x))

(define (divisible? x y) (= (remainder x y) 0))

(define (enumerate-interval low high) 
  (if (> low high) 
      '() 
      (cons low (enumerate-interval (+ low 1) high))))

(define primes
  (cons-stream 2
               (berkeley:stream-filter prime?
                                       (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

; From http://matt.might.net/articles/higher-order-list-operations/
(define (two-arg-reduce op lst)
  (match lst
    ['()             (error "no elements in list")]
    [(list a)         a]
    [(cons hd tl)    (op hd (two-arg-reduce op tl))]))

(define (berkeley:accumulate . args)
  (if (= (length args) 2)
      (apply two-arg-reduce args)
      (apply foldr args))) 

; Export everything, overwriting built-in Racket bindings where necessary

(#%provide
  (all-defined)
  (all-from xrepl)
  (all-from compatibility/mlist)
  (all-from-except (planet dyoo/simply-scheme:2:2) accumulate)
  (rename berkeley:stream-ref stream-ref)
  (rename berkeley:stream-map stream-map)
  (rename berkeley:stream-for-each stream-for-each)
  (rename berkeley:stream-filter stream-filter)
  (rename berkeley:accumulate accumulate)
)
