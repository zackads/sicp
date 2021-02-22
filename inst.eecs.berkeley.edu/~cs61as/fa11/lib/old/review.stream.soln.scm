Stream review problem solutions.

---------------------------------------memoization of a function
;; try them out
;; (annoy-me)
;; (annoy-me)
;; (f)
;; (f)
You should have noticed that `annoy-me' had the same
behavior both times you called it.

On the other hand, `f' only displayed the the message
the first time you called it.  Because `f' is memoized,
after it is called the first time, the result is stored
in the local variable `result'.  Every time after that,
when `f' is called, the value in `result' is returned.

To see the environment diagrams (as opposed to trying to
read my text drawings), run this problem in EnvDraw.
To run EnvDraw, do the following:

run emacs
M-x run-stk
at the STk prompt type:  (envdraw)
Type the procedure definitions into the EnvDraw prompt.



---------------------------------------memoization of streams
;; (define test1 (map identity-with-message ints))
Note that the identity-with-message function was only
applied to 1 (the head of ints).  The cons-stream inside
the function map delayed the rest of the computation.


;; (ss test1 5)
You should have noticed that identity-with-message was
applied to the 2,3,4,5, and 6 of the ints.


;; (ss test1 5)
See how the identity-with-message was not applied to the
2,3,4,5, and 6 this time?  This is because streams are
memoized, and as soon as a tail is forced, the result was
stored in `result'.  Think about why there is a different
local variable `result' for each tail of the stream.


;; (ss test1 10)
`identity-with-message' is applied to 7,8,9,10, and 11
because the first 5 tails of test1 have already been computed.
However, none of the stream past 6 had been computed yet.


---------------------------------------non memoized stream
;; (define test2 (map-w/o-memo identity-with-message new-ints))
Defined just like any other stream.


;; (ss test2 5)
`identity-with-message' is applied to 2,3,4,5, and 6
just like before.

;; (ss test2 5)
And again `identity-with-message' is applied to 2,3,4,5, and 6.
This is because the stream is not memoized, each part of the stream
needs to be recomputed every time.

;; (ss test2 10)
This was just to emphasize the lack of memoization.


;; Extra question:
;; are the following streams memoized:

;; (define test3 (map-w/o-memo identity-with-message ints))
No, because test3 is created by `map-w/o-memo' which creates
a stream that isn't memoized, regardless of whether or not
the input stream is memoized.


;; (define test4 (map identity-with-message new-ints))
Yes, because `map' creates a memoized stream.
(because it uses cons-stream)



---------------------------------------general stream problems
;;; Here are two more ways to write ints,
;;; make sure you understand how they work.

;;(define ints (cons-stream 1 (map 1+ ints)))
Initially, `ints' is a cons pair whose car is a 1,
and the cdr is a promise to compute (map 1+ ints).

ints -------|
            v
        ---------
        |   | __|__\ (delay (map 1+ ints))  ;; this should realy be a function
        | | |   |  /
        --|------
          v
          1

What does (map 1+ ints) return (when it gets forced)?
Because map uses cons-stream, it returns a cons pair whose
car is (1+ (head ints)) or better known as 2, and whose cdr
is a promise to compute (map 1+ (tail ints)).

(tail ints)
  which is
(map 1+ ints)
  returns this cons pair:

        ---------
        |   | __|__\ (delay (map 1+ (tail ints)))
        | | |   |  /
        --|------
          v
          2


What does (map 1+ (tail ints)) return?
Well, a cons pair whose car is (1+ (head (tail ints)))
(tail ints) returns the above cons-pair,
(head (tail ints)) returns 2,
so (1+ (head (tail ints))) gives us 3.

        ---------
        |   | __|__\ (delay (map 1+ (tail (tail ints))))
        | | |   |  /
        --|------
          v
          3

and so on...


;;(define ints (cons-stream 1 (add-streams ones ints)))
This is really just like the above definition of ints,
except that instead of mapping `1+' down the stream, we're
adding a stream of 1's to ints.  Both have the same effect.




---------------------------------------other stream problems
;; one possible solution for make-repeat-stream
(define (make-repeat-stream l)
  (cons-stream (car l)
               (make-repeat-stream (append (cdr l) (list (car l))))))


;; one possible solution for rand-10
(define rand-10 (make-rand-stream 10))
(define (make-rand-stream n)
    (cons-stream (random n)
                 (make-rand-stream n)))

;; one possible solution for the fibonacci numbers
(define fibs (cons-stream 0 (cons-stream 1 (add-streams fibs (tail fibs)))))


;;;;;; Harmonics
(define harmonics (cons-stream 1 (map2 + harmonics inverses)))
(define inverses (map (lambda (x) (/ 1 x)) (tail (tail naturals))))

;;
;; or
;;

(define harmonics2
  (map (lambda (n) (accumulate + 0 (map (lambda (x) (/ 1 x))
					(range 1 n))))
       (tail naturals)))
						 


;;;;;;; rationals
(define ints (ints-from 1))
(define (ints-from n)
  (cons-stream n
               (ints-from (1+ n))))

;; this makes a stream of rational all rational numbers n/x for a given n
;; e.g. (make-row 1) ==>
;;      {1 0.5 0.33333... 0.25 0.2 0.16666... 0.142857... 0.125 ... }
(define (make-row n)
  (map (lambda (x) (/ n x))
       ints))

;; now that we have a function which given a number n creates all rational
;; numbers whose numerator is n, let's map that function over the ints to
;; get a stream of streams.  
;; stream-of-rows has the form:
;; { {1 1/2 1/3 1/4 1/5 ...} {2 2/2 2/3 2/4 2/5 ...} {3 3/2 3/3 3/4 3/5...} ...}
;; thus all the rational numbers are elements of the streams in stream-of-rows
(define stream-of-rows (map make-row ints))

;; now get rid of the nesting streams and create a stream
;; whose elements are all the rational numbers.
(define rationals (flatten stream-of-rows))

