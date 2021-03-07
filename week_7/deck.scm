(load "obj.scm")
(load "simply.scm")

(define ordered-deck '(AH 2H 3H QH KH AS 2S QC KC))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
        (cons card (shuffle (remove card deck))) )))

(define-class (deck)
  (instance-vars (cards (shuffle ordered-deck)))
  (method (deal)
          (let ((top-card (car cards)))
            (begin
              (set! cards (cdr cards))
              top-card)))
  (method (empty?)
          (null? cards)))

(define test-deck (instantiate deck))
(print (ask test-deck 'deal))
(print (ask test-deck 'empty?)) ; False