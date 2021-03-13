#lang racket

(define f
  (let ((invocations 0))
    (lambda (x)
      (set! invocations (+ invocations 1))
      (cond ((= x 0) 0)
            ((even? invocations) 0)
            (else 1)))))
  
(+ (f 0) (f 1)) ;; 0
(+ (f 1) (f 0)) ;; 1