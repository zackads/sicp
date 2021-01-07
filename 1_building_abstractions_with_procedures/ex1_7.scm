; Exercise 1.7
; "The `good-enough?` test used in computing square roots will not be very effective for finding the 
; square roots of very small numbers.  Also, in real computers, arithmetic operations are almost always
; performed with limited precision.  This makes our test inadequate for very large numbers."
;
; Explain these statements, with examples showing how the test fails for small and large numbers.

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                    x)))

; A guess is improved by averaging it with the quotient of the radicand and the old guess
(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

; Good enough is defined here as within the predetermined tolerance of 0.001
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

; Driver code stars here

; "The `good-enough?` test used in computing square roots will not be very effective for finding the 
; square roots of very small numbers."



(sqrt 0.008) ; .0900198385515017, OK (0.0894427191)
(sqrt 0.007) ; .08445065628268553, should be +/- 0.001 of 0.08366600265
(sqrt 0.006) ; .06548128198973399, should be +/- 0.001 of 0.07745966692
(sqrt 0.005) ; .0785456883688794, should be +/- 0.001 of 0.07071067811
(sqrt 0.004) ; .06548128198973399, 0.0632455532
(sqrt 0.003) ; .05815193427238369, 0.05477225575
(sqrt 0.002) ; 5.0131352980478244e-2, 0.04472135955
(sqrt 0.001) ; .04124542607499115, 0.0316227766
(sqrt 0.0009) ; .04030062264654547, should be 0.03

; "Also, in real computers, arithmetic operations are almost always performed with limited precision.  
; This makes our test inadequate for very large numbers."