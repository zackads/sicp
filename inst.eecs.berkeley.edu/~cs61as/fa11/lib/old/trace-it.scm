
; Load this file after loading one of the other interpreters,
; such as mceval.scm, lazy.scm, analyze.scm, or ambeval.scm.


(define (trace-filter x)
    (cond ((tagged-list? x 'procedure)
           (list (car x) (cadr x) (caddr x) '...))
          ((tagged-list? x 'thunk)
           (list (car x) (cadr x) '...))
          ((tagged-list? x 'evaluated-thunk)
           (list (car x) (trace-filter (cadr x))))
          (else x)
    ))

trace
(set-trace-filter trace-filter)
(trace1 mc-eval)
(trace2 mc-apply)
