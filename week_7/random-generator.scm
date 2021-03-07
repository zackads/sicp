(load "obj.scm")
(load "simply.scm")

(define-class (random-generator upper-bound)
  (instance-vars (count 0))
  (method (number)
          (set! count (+ count 1))
          (random upper-bound))
  (method (count)
          count))

(define r10 (instantiate random-generator 10))
(print (ask r10 'number)) ; random number between 0 and 9
(print (ask r10 'count)) ; 1
(print (ask r10 'number)) ; random number between 0 and 9
(print (ask r10 'count)) ; 2
(print (ask r10 'number)) ; random number between 0 and 9
(print (ask r10 'count)) ; 3
(print (ask r10 'number)) ; random number between 0 and 9
(print (ask r10 'count)) ; 4