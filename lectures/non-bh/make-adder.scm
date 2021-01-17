;; Defining a procedure which returns a procedure:

(define (make-adder n)
  (lambda (x) (+ x n)))


;; Applying make-adder to any argument gives us a procedure:

(make-adder 2)


;; To use the returned procedure, we have to either bind some variable
;; to it using define, e.g.

(define 2+ (make-adder 2))

(2+ 5)    ; this will return 7


;; Another possibility is to apply the returned procedure directly by
;; placing the (make-adder ...) expression in the operator position of
;; a combination:

((make-adder 2) 5)    ; also returns 7


;; A procedure returned by make-adder can be used anywhere any other
;; procedure can be used.  We could, for example, pass it as an
;; argument to another function.  Recall the sum procedure from
;; yesterday:

(define (sum a b fn)    ; computes f(a) + f(a + 1) + ... + f(b)
  (if (> a b)
      0
      (+ (fn a)
	 (sum (1+ a) b fn))))

(sum 1 5 (lambda (x) (* x x)))   ; returns 1 * 1 + 2 * 2 + ... + 5 * 5

(sum 1 5 (make-adder 2))    ; returns (1 + 2) + (2 + 2) + ... + (5 + 2)
