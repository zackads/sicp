(load "twenty-one.scm")

;don't worry about how this function works. All you have to know
;is its domain and range
(define (run-test test-num func-name actual-result expected-result)
  (format #t "Testing case ~A for ~A: " test-num func-name)
  (if (not (equal? expected-result actual-result))
      (format #t "Failed.~%Expected: ~A~%Got: ~A~%~%"
              expected-result actual-result)
      (format #t "Passed!~%")))

;; Tests for best-total
(define (test-best-total)
  (run-test 1 "best-total"               ;; Test 1 for best-total
            (best-total '(2c 6d 3s 8h))  ;; Code to run for test 1
            19)                          ;; Expected output

  (run-test 2 "best-total"
            (best-total '(ad 8s)) ;; in this hand the ace counts as 11
            19)

  (run-test 3 "best-total"
            (best-total '(ad 8s 5h)) ;; here it must count as 1 to avoid busting
            14)

  (run-test 4 "best-total"
            (best-total '(ad as 9h)) ;; here one counts as 11 and the other as 1
            21)

  ;; Add more tests here.  Best-total has many different cases, so you
  ;; should have a *lot* of tests.  For example, you may want to test
  ;; that face cards (jacks/queens/kings) work, that aces work at the
  ;; beginning, the end, and the middle of the hand, and so on.

)
(test-best-total)

;; Tests for stop-at-17
(define (test-stop-at-17)
  (run-test 1 "stop-at-17"
            (stop-at-17 '(kh 6d) 'ah)
            #t)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for stop-at-17
;; (test-stop-at-17)


;; Tests for play-n
(define (test-play-n)
  (run-test 1 "play-n"
            (play-n (lambda (x y) #t) 5)
            -5)
  ;; Since play-n is random, it is hard to write good tests for it.  You
  ;; should run play-n and make sure it returns reasonable outputs, but
  ;; you do not need to write more tests here for play-n.
)
;; Uncomment the following line to run tests for play-n
;; (test-play-n)

;; Tests for dealer-sensitive
(define (test-dealer-sensitive)
  (run-test 1 "dealer-sensitive"
            (dealer-sensitive '(4s 3s qh) '5h)
            #f)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for dealer-sensitive
;; (test-dealer-sensitive)


;; Tests for stop-at
(define (test-stop-at)
  (run-test 1 "stop-at"
            ((stop-at 4) '(4d) '7h)
            #f)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for stop-at
;; (test-stop-at)


;; Tests for valentine
(define (test-valentine)
  (run-test 1 "valentine"
            (valentine '(qd 7h) 'jc)
            #t)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for valentine
;; (test-valentine)


;; Tests for suit-strategy
(define (test-suit-strategy)
  (run-test 1 "suit-strategy"
            ((suit-strategy 'c (stop-at 7) (stop-at 10)) '(4c 5d) '9h)
            #t)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for suit-strategy
;; (test-suit-strategy)

;; Tests for valentine2
;; You can copy over your tests for valentine and change them to use
;; valentine2 instead of valentine.
(define (test-valentine2)
  (run-test 1 "valentine2"
            (valentine2 '(qd 7h) 'jc)
            #t)
  ;; Add more tests here
)
;; Uncomment the following line to run tests for valentine2
;; (test-valentine2)


;; Tests for majority
(define (test-majority)
  (run-test 1 "majority"
            ((majority stop-at-17 dealer-sensitive (stop-at 14)) '(kd 6h) '9h)
            #t)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for majority
;; (test-majority)


;; Tests for reckless
(define (test-reckless)
  (run-test 1 "reckless"
            ((reckless (stop-at 12)) '(ad 8s) '10h)
            #t)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for play-n
;; (test-reckless)


;; Tests for jokers
(define (test-joker)
  (run-test 1 "joker"
            (best-total '(2c 6d 3s 8h))
            19)
  ;; Add more tests here

)
;; Uncomment the following lines to run tests for jokers
;; (load "joker.scm")
;; (test-joker)
