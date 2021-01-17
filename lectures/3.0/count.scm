(define-class (counter)
  (instance-vars (count 0))
  (method (next)
    (set! count (+ count 1))
    count) )

(define-class (counter2)
  (instance-vars (count 0))
  (class-vars (total 0))
  (method (next)
    (set! total (+ total 1))
    (set! count (+ count 1))
    (list count total)))

(define-class (counter3)
  (instance-vars (count 0))
  (class-vars (total 0) (counters '()))
  (initialize (set! counters (cons self counters)))
  (method (next)
    (set! total (+ total 1))
    (set! count (+ count 1))
    (list count total)))

(define c11 (instantiate counter))
(define c12 (instantiate counter))

(define c21 (instantiate counter2))
(define c22 (instantiate counter2))

(define c31 (instantiate counter3))
(define c32 (instantiate counter3))

