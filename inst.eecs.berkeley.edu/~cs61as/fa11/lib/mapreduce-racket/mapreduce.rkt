#lang racket

; mapreduce.rkt - Multithreaded MapReduce in Racket
; Allen Guo <allenguo@berkeley.edu>

(provide make-kv-pair
         kv-key
         kv-value
         mapreduce)

; Key-value pair ADT
(define make-kv-pair cons)
(define kv-key car)
(define kv-value cdr)

(define (mapreduce mapper reducer base-case data)
  ; Setup
  (define main-thread (current-thread))   ; We'll need this to get results from other threads
  (define (receive . _) (thread-receive)) ; Variadic thread-receive that discards all args
  ; Map phase
  (for/list ([kv-pair data])                                     ; For each kv-pair in the input..
    (thread (thunk (thread-send main-thread (mapper kv-pair))))) ;  make a new thread to compute `(mapper kv-pair)`
  (define map-results (append-map receive data))                 ; Wait, collect the results, and flatten
  ; Sort phase
  (define grouped (make-hash))                         ; Make hash table mapping sort key to list of values
  (for/list ([result map-results])                     ; For every kv-pair created by the mapper..
    (hash-update! grouped                              ;  update hash table entry in `grouped`
                  (kv-key result)                      ;  for key `(kv-key result)`
                  (curry cons (kv-value result)) '())) ;  so that it contains `(kv-value result)`
  ; Reduce phase
  (hash-for-each grouped              ; For each entry in `grouped`..
                 (lambda (key values) ;  compute the final value by reducing using `reducer` and `base-case`
                   (thread (thunk (thread-send main-thread (make-kv-pair key (foldl reducer base-case values)))))))
  (hash-map grouped receive)) ; Wait, then collect one result for each entry in `grouped`

