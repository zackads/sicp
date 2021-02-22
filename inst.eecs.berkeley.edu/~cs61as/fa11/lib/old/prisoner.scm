;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  OVERVIEW:
;;  
;;
;;  The PLAY-LOOP procedure takes as its arguments two prisoner's 
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds. A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays. A history consists
;;  of a sentence of C's and D's.  Likewise, a strategy procedure 
;;  returns either the word C (for "cooperate") or D ("defect").
;;  We also need a way to find out the player's scores; GET-SCORES
;;  takes two histories and computes the net score for one player.
;;
;;  Note that we are inventing various types of objects: strategies,
;;  histories, etc.  Each type of thing has certain specified
;;  properties. For example, a history is a sentence with zero or
;;  more words, each of which is a C or D.  To help us use these
;;  objects, we write procedures that let us forget the details and
;;  think about things in terms of histories and rounds, not in terms
;;  of sentences and words. (For example, see GET-NTH-FROM-LAST-PLAY,
;;  ADD-TO-HISTORY, etc.) 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop strategy1 strategy2)

  ;; returns final scores

  (define (play-loop-helper strat1 strat2 history1 history2 counter limit)
    (if (= counter limit)
	(final-scores history1 history2 limit)
        (let ((result1 (strat1 history1 history2))
	      (result2 (strat2 history2 history1))) 
					;; note that the strategy's
					;; own history comes first
	  (play-loop-helper strat1 strat2
			    (add-to-history result1 history1)
			    (add-to-history result2 history2)
			    (1+ counter)
			    limit))))            ; end of helper

  (play-loop-helper strategy1 strategy2          ; play-loop body
		    empty-history empty-history
		    0
		    (+ 90 (random 21))))



(define (final-scores history1 history2 num-of-rounds)

  ;; returns average score per round for the two histories

  (se (/ (get-scores history1 history2) num-of-rounds)
      (/ (get-scores history2 history1) num-of-rounds) ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Procedures about histories
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-history '())

(define empty-history? empty?)

(define (add-to-history result history)
  (se history result))

(define (last-play history)
  (last history))

(define (previous-history history)
  (bl history))

(define (get-nth-to-last-play n history)
  (cond ((empty? history) '())
	((= n 1) (last history))
	(else (get-nth-to-last-play (1- n) (butlast history)))))


(define (get-scores my-hist other-hist)

  ;; returns total score for first player
  
  (if (or (empty? my-hist) (empty? other-hist))
      0
      (+ (get-score (first my-hist) (first other-hist))
	 (get-scores (bf my-hist) (bf other-hist)))))

(define (get-score my-play other-play)

  ;; returns the score of the first player for this round

  (let ((round (se my-play other-play)))
    (cond ((equal? round '(C C)) 3)
	  ((equal? round '(C D)) 0)
	  ((equal? round '(D C)) 5)
	  ((equal? round '(D D)) 1) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Some strategies.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (all-defect my-history other-history)
  'D)

(define (poor-trusting-fool my-history other-history)
  'C)

(define (unforgiving my-history other-history)
  (define (ever-defected? history)
    (if (empty-history? history) 
	#f
        (or (equal? (last-play history) 'D)
	    (ever-defected? (previous-history history)))))
  (if (ever-defected? other-history) 'D 'C))

(define (tit-for-tat my-history other-history)
  (if (empty-history? other-history)
      'C
      (last-play other-history)))

(define (random-strategy my-history other-history)
  (nth (random 2) '(C D)))
