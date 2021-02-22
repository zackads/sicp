#lang racket

(require "mapreduce.rkt")
(require "bible-data.rkt")

(define (mapper input-kv-pair)
    (map (lambda (wd) (make-kv-pair wd 1)) (kv-value input-kv-pair)))

(define start (current-inexact-milliseconds))
(define ans (mapreduce mapper + 0 data))
(displayln (- (current-inexact-milliseconds) start))
