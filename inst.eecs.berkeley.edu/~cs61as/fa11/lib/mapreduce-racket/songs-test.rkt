#lang racket

(require "mapreduce.rkt")

(define song1 '(((please please me) i saw her standing there)
                ((please please me) misery)
                ((please please me) please please me)))

(define song2 '(((with the beatles) it wont be long)
                ((with the beatles) all ive got to do)
                ((with the beatles) all my loving)))

(define song3 '(((a hard days night) a hard days night)
                ((a hard days night) i should have known better)
                ((a hard days night) if i fell)))

(define all-songs (append song1 song2 song3))

(define (mapper input-kv-pair)
    (map (lambda (wd) (make-kv-pair wd 1)) (kv-value input-kv-pair)))

(mapreduce mapper + 0 all-songs)
