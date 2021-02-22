;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The PLAY-LOOP procedure takes as its arguments two game
;;  strategies, and plays an iterated game of 25 rounds.
;;  A strategy is a procedure that takes two arguments:
;;  a history of this player's previous plays and 
;;  a history of the other player's previous plays. The strategy
;;  returns one of the words ROCK, PAPER, or SCISSORS.  A history
;;  is a list of previous plays, most recent first.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1
                          history0 history1
                          score0 score1
                          rounds)
    (cond ((= rounds 0) (list score0 score1))
          (else (YOU-WRITE-THIS-PART)) ))
  (play-loop-iter strat0 strat1 '() '() 0 0 25) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This predicate procedure takes two plays as arguments and
;; returns TRUE if the first beats the second.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (beats? play0 play1)
  (cond ((equal? play0 'paper) (equal? play1 'rock))
        ((equal? play0 'scissors) (equal? play1 'paper))
        ((equal? play0 'rock) (equal? play1 'scissors)) ))

;; A sampler of strategies

(define (tit-for-tat my-history other-history)
  (if (empty? my-history)
      'rock ;; arbitrary -- could be a random choice here
      (first other-history) ))

(define (random-strategy my-history other-history)
  (nth (random 3) '(rock paper scissors)) )

(define (heavy-metal my-history other-history)
  'scissors)

(define (hard-rock my-history other-history)
  'rock)
