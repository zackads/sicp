; first version -- no data abstraction

(define (total-hand hand)
  (if (empty? hand)
      0
      (+ (butlast (last hand))
	 (total-hand (butlast hand)) )))


(total-hand '(3h 10c 4d))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; second version -- add selectors

(define (total-hand hand)
  (if (empty? hand)
      0
      (+ (card-rank (one-card hand))
	 (total-hand (remaining-cards hand)) )))

(define card-rank butlast)
(define card-suit last)

(define one-card last)
(define remaining-cards butlast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; third version -- add constructors

(define (make-card rank suit)
  (word rank (first suit)) )

(define make-hand se)


(total-hand (make-hand (make-card 3 'heart)
		       (make-card 10 'club)
		       (make-card 4 'diamond) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fourth version -- change implementation

(define (make-card rank suit)
  (cond ((equal? suit 'heart) rank)
	((equal? suit 'spade) (+ rank 13))
	((equal? suit 'diamond) (+ rank 26))
	((equal? suit 'club) (+ rank 39))
	(else (error "say what?")) ))

(define (card-rank card)
  (remainder card 13))

(define (card-suit card)
  (nth (quotient card 13) '(heart spade diamond club)))

