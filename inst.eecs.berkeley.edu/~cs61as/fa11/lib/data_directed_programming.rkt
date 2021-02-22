#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))


(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

;;YOU CANNOT CLEAR THE TABLE. IF YOU WOULD LIKE TO DO, YOU MUST RESTART RACKET AND RELOAD YOUR FILES