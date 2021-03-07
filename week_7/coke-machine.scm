(load "obj.scm")
(load "simply.scm")

(define-class (coke-machine capacity unit-price)
  (instance-vars (cokes 0) (machine-money 0))
  (method (deposit user-money)
          (set! machine-money (+ machine-money user-money)))
  (method (coke)
          (cond ((< machine-money unit-price) (print "Not enough money"))
                ((equal? cokes 0) (print "Machine empty"))
                (else (set! machine-money (- machine-money unit-price))
                      (set! cokes (- cokes 1))
                      machine-money)))
  (method (fill new-cokes)
          (set! cokes (+ cokes new-cokes))))

(define my-machine (instantiate coke-machine 80 70))
(print (ask my-machine 'fill 60))
(print (ask my-machine 'deposit 25))
(print (ask my-machine 'coke)) ;; NOT ENOUGH MONEY
(print (ask my-machine 'deposit 25)) ;; Now there’s 50 cents in there.
(print (ask my-machine 'deposit 25)) ;; Now there’s 75 cents.
(print (ask my-machine 'coke)) ;; return val is 5 cents change.