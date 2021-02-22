;; This is a simulation of the Monty Hall problem, created during Su09 (Min Xu)
;; Usage: (count-wins (play-monty STRAT 1000))

(define (monty-hall strat)
  (let ((prize (random 3))
        (chosen (random 3)))
    (if (strat chosen (rand-open chosen prize 3))
        (not (= prize chosen))
        (= prize chosen))))

(define (rand-open chosen prize total)
  (let ((rand-door (random total)))
    (if (or (= rand-door prize) (= rand-door chosen))
        (rand-open chosen prize total)
        rand-door)))

(define (play-monty strat rounds)
  (if (= rounds 0) '()
      (cons (monty-hall strat) (play-monty strat (- rounds 1)))))

(define (count-wins result)
  (/ (length (keep (lambda (x) x) result))
     (length result)))


(define (always-switch chosen open-door)
  #t)

(define (sometimes-switch chosen open-door)
  (and (= chosen 1) (= open-door 3)))
