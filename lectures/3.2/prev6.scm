(define-class (previous)
  (class-vars (glob 'first-time))
  (locals (old 'first-time))
  (method (local arg)
    (let ((result old))
      (set! old arg)
      result) )
  (method (global arg)
    (let ((result glob))
      (set! glob arg)
      result) ) )



