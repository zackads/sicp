;;; The integers.

(define (ints-from n)
  (cons-stream n (ints-from (+ n 1))))

(define integers (ints-from 1))

;;; pairs of integers, A&S version

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;; Louis Reasoner's version, ex. 3.68

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (louis-pairs (stream-cdr s) (stream-cdr t))))

;;; Fixing Louis's version with explicit DELAY and FORCE

(define (fixed-pairs s t)
  (interleave-delayed
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (delay (fixed-pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (interleave-delayed (force delayed-s2)
                                       (delay (stream-cdr s1))))))

