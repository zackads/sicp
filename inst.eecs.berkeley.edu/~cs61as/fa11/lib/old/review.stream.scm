;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The following is to show how a memoization works
;;;; on a function (of no arguments).

;;;; Here is the built in procedure which memoizes the function:

(define (memo-proc proc)
  (let ((already-run? #f) (result '()))
    (lambda ()
      (if (not already-run?)
          (sequence (set! result (proc))
                    (set! already-run? #t)
                    result)
          result))))

;; example procedure
(define (annoy-me)
  (display "This is annoying!\n")
  (display "This is annoying!\n")
  (display "This is annoying!\n")
  3)

;; f is a memoized version of annoy-me
(define f (memo-proc annoy-me))

;; try them out
(annoy-me)
(annoy-me)
(f)
(f)

;; See the difference?
;; To fully understand it, draw the environment diagrams.
;; (and you thought this was just reviewing streams...)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The following is demonstrates memoization in streams

;; For reference
;;
;;  When you type (cons-stream x y)
;;  that translates to
;;     (cons x (delay y))
;;
;;  which is the same as:
;;     (cons x (memo-proc (lambda () y)))


;; useful function
(define (ints-from n)
  (cons-stream n
       (ints-from (1+ n))))

;; the integers
(define ints (ints-from 1))

;; function that prints a message
(define (identity-with-message n) 
  (display "this fcn applied to the number ")
  (display n)
  (newline)
  n)

(identity-with-message 3)    ;; see what it does


;;;; Check out how test behaves...
(define test1 (map identity-with-message ints))

(ss test1 5)
(ss test1 5)
(ss test1 10)


;;;; This is the same as above, but wit a non memoized stream

;; The stream has been explicitly written w/out the memoization
(define (ints-from-w/o-memo n)
  (cons n
        (lambda () (ints-from-w/o-memo (1+ n)))))

;; write a map function which doesn't use cons-stream
(define (map-w/o-memo fcn stream)
  (cons (fcn (head stream))
        (lambda () (map-w/o-memo fcn (tail stream)))))


;; non memoized stream
(define new-ints (ints-from-w/o-memo 1))

;; non memoized stream
(define test2 (map-w/o-memo identity-with-message new-ints))


;;;; Check out how this test behaves
(ss test2 5)
(ss test2 5)
(ss test2 10)



;; NOTE: new-ints wasn't really needed,
;; but I didn't want to confuse you.

;; Extra question:
;; are the following streams memoized:

(define test3 (map-w/o-memo identity-with-message ints))
(define test4 (map identity-with-message new-ints))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following are general stream problems

;;;; some utility functions to be used below
(define (add-streams s1 s2)
  (cons-stream (+ (head s1) (head s2))
               (add-streams (tail s1) (tail s2))))
(define ones (cons-stream 1 ones))



;;; Here are two more ways to write ints,
;;; make sure you understand how they work.

(define ints (cons-stream 1 (map 1+ ints)))
(define ints (cons-stream 1 (add-streams ones ints)))



;; Write a function make-repeat-stream which takes a list
;; and returns a stream which repeats the elements of the
;; list over and over and over...

> (define one-two-three (make-repeat-stream '(1 2 3)))
one-two-three
> (ss one-two-three)
{1 2 3 1 2 3 1 2 3 1 ...}


;; Good streams to try to write:

rand-10    (stream of random numbers b/w 1 and 10  e.g. {1 7 3 9 8 2 10 4 2 3 3 7 6 ... }
fibonacci  (contains all fibonacci numbers (don't look at book))
rationals  (contains all rational numbers)


;; A question off MT#3 from last semester:
Let
       n
      ___
      \     1
Sn =   >   ---
      /     x
      ---
      x=1
      
Define a stream named harmonics, which contains all such Sn

e.g.

> (ss harmonics)
{1 1.5 1.833333 2.083333 2.283333 2.45 ... }

that's   1  1+1/2  1+1/2+1/3  1+1/2+1/3+1/4 ...

Harder question,
do it w/out writing your own recursive procedures.

You are allowed to use the following functions:
map, append-streams, interleave, flatten, accumulate
(even though they are recursive)

ie.

;; This definition of ints uses explicit
;; recursion b/c ints-from calls itself.
(define ints (ints-from 1))
(define (ints-from n)
  (cons-stream n
	       (ints-from (1+ n))))

;; This solution for ints is ok:
(define ints (cons-stream 1 (add-streams ones ints)))
