#lang racket/base

(require berkeley)

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
          ((< (best-total dealer-hand-so-far) 17)
           (play-dealer customer-hand
                        (se dealer-hand-so-far (first rest-of-deck))
                        (bf rest-of-deck)))
          ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
          ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
          (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
          ((strategy customer-hand-so-far dealer-up-card)
           (play-customer (se customer-hand-so-far (first rest-of-deck))
                          dealer-up-card
                          (bf rest-of-deck)))
          (else
           (play-dealer customer-hand-so-far
                        (se dealer-up-card (first rest-of-deck))
                        (bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
                   (first (bf (bf deck)))
                   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
        deck
        (move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (best-value card current-sum)
  (define (ace-value) (if (<= (+ 11 current-sum) 21) 11 1))
  (cond ((equal? (butlast card) 'A) (ace-value))
        ((or
          (equal? (butlast card) 'K) 
          (equal? (butlast card) 'Q) 
          (equal? (butlast card) 'J))
         10)
        (else (butlast card))))

(define (sum-filter filter accumulator transform sent)
  (if (equal? sent '())
      accumulator
      (if (filter (first sent))
          (sum-filter filter (+ accumulator (transform (first sent) accumulator)) transform (butfirst sent))
          (sum-filter filter accumulator transform (butfirst sent)) )))

; filter, map and sum not used: I abstracted these to think about how I could generalise out the constituent
; parts of sum-filter.  I got distracted and didn't use them

(define (filter sent predicate?)
  (if (equal? sent '())
      '()
      (if (predicate? (first sent))
          (sentence (first sent) (filter (butfirst sent) predicate?))
          (filter (butfirst sent) predicate?))))

(define (map sent procedure)
  (if (equal? sent '())
      '()
      (sentence (procedure (first sent)) (map (butfirst sent) procedure))))

(define (sum sent)
  (define (sum-iter accumulator sent)
    (if (equal? sent '())
        accumulator
        (sum-iter (+ accumulator (first sent)) (butfirst sent))))
  (sum-iter 0 sent))

(define (best-total cards)
  (define (ace? card) (equal? (first card) 'a))
  (define (not-ace? card) (not (ace? card)))
  (sum-filter ace? (sum-filter not-ace? 0 best-value cards) best-value cards))

(define (contains? sent pred)
  (if (equal? sent '())
      #f
      (if (pred (first sent))
          #t
          (contains? (butfirst sent) pred))))

; Player strategies
(define (stop-at n)
  (lambda (customer-hand-so-far dealer-up-card)
    (< (best-total (sentence customer-hand-so-far dealer-up-card)) n)))

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  ((stop-at 17) customer-hand-so-far dealer-up-card))
  
(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  (or (and (>= (best-value dealer-up-card 10) 7)
           ((stop-at 17) customer-hand-so-far dealer-up-card))
      (and
       (>= 2 (best-value dealer-up-card 1))
       (<= 6 (best-value dealer-up-card 1))
       ((stop-at 12) customer-hand-so-far dealer-up-card))))

(define (suit-strategy suit strategy-if-suit-found strategy-if-suit-not-found)
  (lambda (customer-hand-so-far dealer-up-card)
    (if (contains? customer-hand-so-far (lambda (card) (equal? (last card) suit)))
        (strategy-if-suit-found customer-hand-so-far dealer-up-card)
        (strategy-if-suit-not-found customer-hand-so-far dealer-up-card))))
  
(define (valentine customer-hand-so-far dealer-up-card)
  ((suit-strategy 'H (stop-at 19) (stop-at 17)) customer-hand-so-far dealer-up-card)) 

(define (majority strategy1 strategy2 strategy3)
  (lambda (customer-hand-so-far dealer-up-card)
    (or (and (strategy1 customer-hand-so-far dealer-up-card)
             (strategy2 customer-hand-so-far dealer-up-card))
        (and (strategy1 customer-hand-so-far dealer-up-card)
             (strategy3 customer-hand-so-far dealer-up-card))
        (and (strategy2 customer-hand-so-far dealer-up-card)
             (strategy3 customer-hand-so-far dealer-up-card)) )))

(define (reckless strategy)
  (lambda (customer-hand-so-far dealer-up-card)
    (strategy (butlast customer-hand-so-far) dealer-up-card)))

(define (play-n strategy n)
  (define (accumulator acc i)
    (if (equal? i 0)
        acc
        (accumulator (+ acc (twenty-one strategy)) (- i 1))))
  
  (accumulator 0 n))

(set! suit-strategy suit-strategy)
(set! contains? contains?)
(set! stop-at stop-at)
; (trace valentine suit-strategy contains? stop-at)

(play-n (reckless valentine) 100)

