#lang racket/base

(require berkeley)

(define (vowel? letter)
  (member? letter '(a e i o u)))

(define (chop-consecutive-vowels word)
  (if (and (vowel? (first word)) (vowel? (first (butfirst word))))
      (butfirst word)
      (word)))

(define (count pred? word)
    (cond ((empty? word) 0)
          ((pred? (first word)) (+ 1 (count pred? (butfirst word))))
          (else (count pred? (butfirst word)))))
  

(define (syllables word)
  (count vowel? (chop-consecutive-vowels word)))