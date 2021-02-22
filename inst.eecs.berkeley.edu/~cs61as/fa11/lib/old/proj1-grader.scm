(load "twenty-one.scm")

(define (run-test test-num func-name actual-result expected-result)
  (format #t "Testing case ~A for ~A: " test-num func-name)
  (if (not (equal? expected-result actual-result))
      (format #t "Failed.~%Expected: ~A~%Got: ~A~%~%"
	      expected-result actual-result)
      (format #t "Passed!~%")))

;; Tests for best-total
(run-test 1 "best-total"               ;; Test 1 for best-total
	  (best-total '(2c 6d 3s 8h))  ;; Code to run for test 1
	  19)                          ;; Expected output

(run-test 2 "best-total"
	  (best-total '(3s ah 7s 3h))
	  14)
;; Add more tests here.  Best-total has many different cases, so you
;; should have a *lot* of tests.


;; Tests for stop-at-17
(run-test 1 "stop-at-17"
	  (stop-at-17 '(kh 7d) 'ah)
	  #f)
;; Add more tests here


;; Tests for play-n
(run-test 1 "play-n"
	  (play-n (lambda (x y) #t) 10)
	  -10)
;; Since play-n is random, it is hard to write good tests for it.  You
;; should run play-n and make sure it returns reasonable outputs, but
;; you do not need to write more tests here for play-n.


;; Tests for dealer-sensitive
(run-test 1 "dealer-sensitive"
	  (dealer-sensitive '(4s 3s qh) '5h)
	  #f)
;; Add more tests here


;; Tests for stop-at
(run-test 1 "stop-at"
	  ((stop-at 4) '(4d) '7h)
	  #f)
;; Add more tests here


;; Tests for valentine
(run-test 1 "valentine"
	  (valentine '(qd 7h) 'jc)
	  #t)
;; Add more tests here


;; Tests for suit-strategy
(run-test 1 "suit-strategy"
	  ((suit-strategy 'c (stop-at 7) (stop-at 10)) '(4c 5d) '9h)
	  #t)
;; Add more tests here

;; Tests for valentine2
;; You can copy over the tests for valentine and change them to use
;; valentine2 instead of valentine.
(run-test 1 "valentine2"
	  (valentine2 '(qd 7h) 'jc)
	  #t)


;; Tests for majority
(run-test 1 "majority"
	  ((majority stop-at-17 dealer-sensitive (stop-at 14)) '(kd 6h) '9h)
	  #t)
;; Add more tests here


;; Tests for reckless
(run-test 1 "reckless"
	  ((reckless (stop-at 12)) '(ad 8s) '10h)
	  #t)
;; Add more tests here


;; Tests for jokers
;; Add more tests here
