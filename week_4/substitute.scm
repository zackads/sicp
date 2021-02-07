#lang racket/base

(require berkeley)

(define (substitute lst old-word new-word)
  (define (replace word)
    (if (list? word)
        (substitute word old-word new-word)
        (if (equal? word old-word)
            new-word
            word)))
  
  (map replace lst))

; Reusing substitute
(define (substitute2 lst old-words new-words)
  (if (empty? old-words)
      lst
      (substitute2
       (substitute lst (car old-words) (car new-words))
       (cdr old-words)
       (cdr new-words))))