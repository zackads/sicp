; Book's version of FAST-EXPT

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))



; A solution to the homework (iterative FAST-EXPT)

(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (iter a (square b) (/ n 2)))
	  (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))


; Same procedure with loop invariant documented

(define (fast-expt b n)
  (define (iter a b n)
    ;;;;; Invariant: a * b\^{}n
    (cond ((= n 0) a)
	  ((even? n) (iter a (square b) (/ n 2)))
	  (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))


; Same procedure with inner parameter names changed, more documentation

(define (fast-expt b n)
  (define (iter a bb nn)
    ;;;;; Invariant:          a * bb\^{}nn = b\^{}n
    ;;;;; Starting condition: a=1, bb=b, nn=n
    ;;;;; Ending condition:   nn=0, therefore bb\^{}nn=1, therefore a=b\^{}n
    (cond ((= nn 0) a)
	  ((even? nn) (iter a (square bb) (/ nn 2)))
	  (else (iter (* a bb) bb (- nn 1)))))
  (iter 1 b n))


; More assertions about why we think this process terminates

(define (fast-expt b n)
  ;;;;; Requirement:       n is a positive integer.
  (define (iter a bb nn)
    ;;;;; Invariant:          a * bb\^{}nn = b\^{}n
    ;;;;; Starting condition: a=1, bb=b, nn=n
    ;;;;; Ending condition:   nn=0, therefore bb\^{}nn=1, therefore a=b\^{}n
    ;;
    ;;;;; Assertion: nn is always a nonnegative integer.
    ;;;;; Assertion: The end condition nn=0 must happen within n steps.
    (cond ((= nn 0) a)
	  ((even? nn) (iter a (square bb) (/ nn 2)))
	  (else (iter (* a bb) bb (- nn 1)))))
  (iter 1 b n))


; Now we modify the procedure.  Which comment is now false?

(define (fast-expt b n)
  ;;;;; Requirement:       n is a positive integer.
  (define (iter a bb nn)
    ;;;;; Invariant:          a * bb\^{}nn = b\^{}n
    ;;;;; Starting condition: a=1, bb=b, nn=n
    ;;;;; Ending condition:   nn=0, therefore bb\^{}nn=1, therefore a=b\^{}n
    ;;
    ;;;;; Assertion: nn is always a nonnegative integer.
    ;;;;; Assertion: The end condition nn=0 must happen within n steps.
    (cond ((= nn 0) a)
	  ((even? nn) (iter a (square bb) (/ nn 2)))
	  (else (iter (* a bb) bb (- nn 1)))))
  (IF (< N 0)
      (/ 1 (ITER 1 B (- N)))
      (iter 1 b n)))
