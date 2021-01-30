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
  (cond ((equal? (butlast card) 'a) (ace-value))
        ((or
          (equal? (butlast card) 'k)
          (equal? (butlast card) 'q)
          (equal? (butlast card) 'j))
         10)
        (else (butlast card))))

(define (sum-filter filter accumulator transform sent)
  (if (equal? sent '())
      accumulator
      (if (filter (first sent))
          (sum-filter filter (+ accumulator (transform (first sent) accumulator)) transform (butfirst sent))
          (sum-filter filter accumulator transform (butfirst sent)) )))

(define (ace? card) (equal? (first card) 'a))
(define (not-ace? card) (not (ace? card)))

(define (best-total cards)
  (sum-filter ace? (sum-filter not-ace? 0 best-value cards) best-value cards))

(set! best-total best-total) ; allow tracing on these functions
(set! sum-filter sum-filter)
(set! best-value best-value)
; (trace best-total sum-filter best-value)

(best-total '(ad 8s)) ; 19, ok
(best-total '(ad 8s 5h)); 14, ok
(best-total '(ad as 9h)); 21, ok
(best-total '(10s 5h)); 15, ok
(best-total '(10s 10h as)); 21, ok
(best-total '(qs jh as)); 21, ok
(best-total '(kd jc as)); 21, ok
(best-total '(kd as)); 21, ok
(best-total '(7d as)); 18, ok