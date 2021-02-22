;; TESTED CODE - Proof of concept for an iterated prisoner's dilemma project.

;; A strategy returns #t for cooperating, and #f for defecting.
(define (play-game game strat1 strat2 iterations)
  (define (loop first-choices second-choices first-score second-score counter)
    (if (= counter 0)
        (se first-score second-score)
        (let ((choice1 (strat1 first-choices second-choices game))
              (choice2 (strat2 second-choices first-choices game)))
          (loop (se first-choices choice1)
                (se second-choices choice2)
                (+ first-score (first-value game choice1 choice2))
                (+ second-score (second-value game choice1 choice2))
                (- counter 1)))))
  (loop '() '() 0 0 iterations))

;; A game is a sentence of 4 numbers.
;; First number:  Value for both players if both cooperate.
;; Second number:  Value for cooperating player if one cooperates and one defects.
;; Third number: Value for defecting player if one cooperates and one defects.
;; Fourth number: Value for both players if both defect.
;; (cc, cd, dc, dd)
(define classic-game '(3 0 5 1))

(define (my-value game choice1 choice2)
  (item (- (if (eq? choice1 'c) 2 4)
           (if (eq? choice2 'c) 1 0))
        game))

(define (other-value game my-choice other-choice)
  (my-value game other-choice my-choice))

(define first-value my-value)
(define second-value other-value)

(define rand-precision (expt 10 6))
(define (rand) (/ (random rand-precision) rand-precision))

;;; Exercises

(define (trusting-fool a b c) 'c)

(define (cynic a b c) 'd)

(define (drunkard a b c)
  (if (= (random 2) 0) 'c 'd))

(define (tit-for-tat my-choices opponent-choices game)
  (if (empty? opponent-choices)
      'c
      (last opponent-choices)))

;; (define (tit-for-tat my-choices opponent-choices game)
;;   (or (empty? opponent-choices) (last opponent-choices)))

(define (expected-best my-choices opponent-choices game)
  (> (+ (my-value game 'c 'c) (my-value game 'c 'd))
     (+ (my-value game 'd 'c) (my-value game 'd 'd))))

;; Does whatever the majority of the strategies says.
(define (majority s1 s2 s3)
  (lambda (mine other game)
    (if (>= (length (keep (lambda (x) (eq? x 'c))
                          (se (s1 mine other game)
                              (s2 mine other game)
                              (s3 mine other game))))
            2)
        'c 'd)))

;; Not too hard to understand solution for nice-guy.
;; Cooperates the first time and cooperates if s would say to cooperate
;; If s says to defect, checks what s would do on the previous iteration, and does that instead.
;; So, it cooperates one more time than s would.
(define (nice-guy s)
  (lambda (mine opponent game)
    (cond ((empty? mine) 'c)
          ((eq? (s mine opponent game) 'c) 'c)
          (else (s (bl mine) (bl opponent) game)))))

;; The rational guy knows that people both cooperate and defect.
;; So, he will cooperate if it is significantly better than defecting.
;; If it isn't good enough, he will just defect.
(define (rational-guy factor)
  (lambda (mine opponent game)
    (if (> (my-value game 'c 'c) (* factor (my-value game 'd 'd)))
        'c
        'd)))


;; The coin flipper lets a biased coin decide his choice.
;; Returns a strategy that cooperates with probability p.
(define (coin-flipper p)
  (lambda (mine opponent game)
    (if (<= (rand) p)
        'c
        'd)))


;; Show how to write CYNIC and TRUSTING-FOOL in terms of COIN-FLIPPER
(define cynic (coin-flipper 0))
(define trusting-fool (coin-flipper 1))

;; Take a strategies S and T and probability P
;; Play strategy S with probability P, T with probability (1-P)
(define (mixed s t p)
  (lambda (mine opponent game)
    (if (<= (rand) p)
        (S mine opponent game)
        (T mine opponent game))))

;; Show how to write coin-flipper in terms of mixed
(define (coin-flipper2 p)
  (mixed trusting-fool cynic p))


;; MATCHER picks a random move from opponent history.
;; Starts out by cooperating.
(define (matcher mine other game)
  (if (empty? other)
      'c
      (item (1+ (random (count other))) other)))


;; DIFFERENT-START takes a strategy S and a move M.
;; It returns a strategy that behaves just like S,
;; except that it starts with move M.
(define (different-start s m)
  (lambda (mine other game)
    (if (empty? other)
        m
        (s mine other game))))

;; Use DIFFERENT-START to make MEAN-MATCHER, which works like MATCHER,
;; but starts out by defecting.
(define mean-matcher (different-start matcher 'd))

;; A few strategies from here:
;; http://www.iterated-prisoners-dilemma.net/prisoners-dilemma-strategies.shtml

;; GRUDGER cooperates until the opponent defects, and then always defects.
(define (grudger mine other game)
  (if (member? other 'd) 'd 'c))

;; Indecisive guy alternates cooperating and defecting.
;; Starts out by cooperating.
(define (indecisive mine other game)
  (if (even? (count mine)) 'c 'd))
