(define old 'first-time)

(define (previous arg)
  (let ((result old))
    (set! old arg)
    result))



