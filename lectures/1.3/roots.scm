;;; Note: all versions work only for quadratics with real roots

;;; Straightforward but slow way:

(define (roots a b c)
  (se (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
      (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) ))


;;; Using a subprocedure to eliminate the repeated computation:

(define (roots a b c)
  (define (roots1 d)
    (se (/ (+ (- b) d) (* 2 a))
	(/ (- (- b) d) (* 2 a)) ))
  (roots1 (sqrt (- (* b b) (* 4 a c)))) )


;;; Using lambda to avoid naming the subprocedure:

(define (roots a b c)
  ((lambda (d)
     (se (/ (+ (- b) d) (* 2 a))
	 (/ (- (- b) d) (* 2 a)) ))
   (sqrt (- (* b b) (* 4 a c))) ))


;;; Using let to rearrange the above:

(define (roots a b c)
  (let ((d (sqrt (- (* b b) (* 4 a c)))))
    (se (/ (+ (- b) d) (* 2 a))
	(/ (- (- b) d) (* 2 a)) )))



;;; More optimization:

(define (roots a b c)
  (let ((d (sqrt (- (* b b) (* 4 a c))))
	(-b (- b))
	(2a (* 2 a)))
    (se (/ (+ -b d) 2a)
	(/ (- -b d) 2a) )))



