;; review notes for 4/10/98
;;can you deadlock on this?

(parallel-execute  (exchange-account a b)(exchange-account a c)) ?


what do these do:

(define a (cons-stream a a))

(define a (cons-stream 'a a))
(show-stream a)
(define a (cons-stream a a))
(show-stream a)


what is the difference between eq?  and equal?

(eq? () '())


(pair? ())

;; pack a finite stream with infinite newelements on end

(define (pack-stream s newelement)
  (if (stream-null? s)
     (cons-stream newelement(pack-stream s newelement))
    (cons-stream (stream-car s) (pack-stream (stream-cdr s) newelement))))

;or (buggy)

(define (pack-stream s newelement)
  (if (stream-null? s) (let ((a (cons-stream newelement a))) a)
    (cons-stream (stream-car s) (pack-stream (stream-cdr s) newelement))))


; or (still buggy)

(define (pack-stream s newelement)
  (if (stream-null? s) (let ((a nil))
                             (set! a (cons-stream newelement a)))
    (cons-stream (stream-car s) (pack-stream (stream-cdr s) newelement))))
; ok

(define (pack-stream s newelement)
  (if (stream-null? s) (let ((a nil))
                         (set! a (cons-stream newelement a))
                         a)
      (cons-stream (stream-car s) (pack-stream (stream-cdr s) newelement))))


