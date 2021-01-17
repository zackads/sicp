;; Data types:
;;
;; RANK		One of the following words: A 2 3 4 5 6 7 8 9 10 J Q K
;;		  representing Ace, Two, Three, ... Jack, Queen, King.
;;
;; SUIT		One of the following words: H S D C
;;		  representing Hearts, Spades, Diamonds, Clubs.
;;
;; CARD		A word consisting of a RANK followed by a SUIT
;;		  e.g., 10H for the Ten of Hearts.
;;
;; HAND		A sentence of CARDs
;;
;; STRATEGY	A procedure that takes two arguments:
;;		    a HAND (the player's hand)
;;		    a CARD (the dealer's visible card)
;;		and returns a Boolean:
;;		    #T if the player should take another card
;;		    #F if the player should not take another card



(define (stop-at-17 my-hand dealer-up-card)
  (< (best-total my-hand) 17))


(define (stop-at-17 my-hand dealer-up-card)
  ;; returns a Boolean
  (< (best-total my-hand) 17))


(define (stop-at-17 my-hand dealer-up-card)
  ;; this is a strategy procedure
  (< (best-total my-hand) 17))


(define (stop-at-17-strategy my-hand dealer-up-card)
  (< (best-total my-hand) 17))




(define (stop-at n)				;; correct
  (lambda (my-hand dealer-up-card)
    (< (best-count my-hand) n)))

(define (stop-at n my-hand dealer-up-card)	;; wrong
  (< (best-count my-hand) n))
