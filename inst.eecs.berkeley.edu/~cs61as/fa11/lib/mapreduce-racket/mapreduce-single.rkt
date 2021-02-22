#lang racket

; mapreduce.rkt - Single-Threaded MapReduce in Racket
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
  ; Map phase
  (define map-results (foldl append '() (map mapper data)))
  ; Sort phase
  (define grouped (make-hash))                         
  (for/list ([result map-results])                     
    (hash-update! grouped                              
                  (kv-key result)                      
                  (curry cons (kv-value result)) '())) 
  ; Reduce phase
  (hash-map grouped (lambda (key values) (make-kv-pair key (foldl reducer base-case values)))))

