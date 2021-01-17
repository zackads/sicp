;;;;  State variables -- count example.

;;------------------------
;; Global state variables.

(define counter 0)

(define (count)
  (set! counter (+ counter 1))
  counter)


;;---------------------------------------------------
;; Local state variables -- first try (DOESN'T WORK).

(define (count)
  (let ((counter 0))
    (set! counter (+ counter 1))
    counter))


;;------------------------------------------
;; Local state variables -- working version.

(define count
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))

(define count (lambda()
		(let ((counter 0)) (set! counter (+ counter 1)) counter)))


;;-------------------------------------------
;; Multiple copies of a local state variable.

(define (make-count init) ;; sort of like let(( localinit init)
  (lambda ()
    (set! init (+ init 1))
    init))




