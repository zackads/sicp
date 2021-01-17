;;;;  State variables -- "previous" example.

;;------------------------
;; Global state variables.

(define old 'first-time)

(define (previous arg)
  (let ((temp old))
    (set! old arg)
    temp))


;;-----------------------
;; Local state variables.

(define previous
  (let ((old 'first-time))
    (lambda (arg)
      (let ((temp old))
        (set! old arg)
        temp))))


;;-------------------------------------------
;; Multiple copies of a local state variable.

(define make-previous
  (lambda ()
    (let ((old 'first-time))
      (lambda (arg)
	(let ((temp old))
	  (set! old arg)
	  temp)))))
