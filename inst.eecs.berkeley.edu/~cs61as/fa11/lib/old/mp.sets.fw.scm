; *****
; Message-passing implementation of a set of integers as a simple list.

(define (new-list-set element-list)
  
  (define (respond-to-msg msg)
    (cond
      ((equal? msg 'member?) member?)
      ((equal? msg 'with-element) with-element)
      ((equal? msg 'elements) (lambda ( ) element-list))
      (else (error "list set: unknown msg")) ) )
  
  (define (member? x)
    (member x element-list) )
  
  (define (with-element x)
    (if (member? x)
        respond-to-msg
        (new-list-set (cons x element-list)) ) )
  
  (define (elements)
    element-list)
  
  respond-to-msg)

; *****
; Message-passing implementation of a set of integers as a sorted list
; of intervals.

(define (new-intvls-set element-list)
  ... )

; *****
; Generic operations on sets.

(define (member? x set)
  ((set 'member?) x) )

(define (with-element x set)
  ((set 'with-element) x) )

(define (elements set)
  ((set 'elements)) )

; *****
; Utility functions.

(define (find-if p L)
  (cond
    ((null? L) #f)
    ((p (car L)) (car L))
    (else (find-if p (cdr L))) ) )