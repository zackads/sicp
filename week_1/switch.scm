#lang racket

(require berkeley)

(define (replace w)
  (cond ((equal? w 'i) 'you)
        ((equal? w 'I) 'you)
        ((equal? w 'me) 'you)
        ((equal? w 'you) 'me)
        (else w) ))

(define (replace-you-with-i w)
  (cond ((equal? w 'you) 'I)
        ((equal? w 'You) 'I)
        (else w)))

(define (switch s)
  (sentence
   (replace-you-with-i (first s))
   (switch-iter (butfirst s))))
                       
(define (switch-iter s)
  (if (empty? s)
      '()
      (sentence
       (replace (first s))
       (switch-iter (butfirst s)))))
  