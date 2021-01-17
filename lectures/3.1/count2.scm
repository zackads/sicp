(define count
  (let ((result 0))
    (lambda ()
      (set! result (+ result 1))
      result)))



