; *****
; Functions that implement data-directed programming (from Concrete Abstractions).

(define (make-type name operation-table)
    (cons name operation-table) )

(define type-name car)

(define type-operation-table cdr)

(define (operate operation-name value)
  (table-find 
    (type-operation-table (type value))
    operation-name
    (lambda (procedure) (procedure (contents value)))
    (lambda ( ) 
      (error 
        "No way of doing operation on type" 
        operation-name 
        (type-name (type value)) ) ) ) )

(define (make-table keys values) 
  (map list keys values) )

(define (table-find table key what-if-found what-if-not)
  (let ((result (assoc key table)))
    (if result (what-if-found (cadr result)) (what-if-not)) ) )

; *****
; Set up of types for data-directed programming.

(define list-set-type
  (make-type 
    'list
    ... ) )

(define intvls-set-type
  (make-type
    'intvls
    ... ) )

; *****
; Generic operations on sets.

(define (member? x set)
  ((operate 'member? set) x) )

(define (with-element x set)
  ((operate 'with-element set) x) )

(define (elements set)
  ((operate 'elements set)) )

; *****
; Utility functions.

(define (find-if p L)
  (cond
    ((null? L) #f)
    ((p (car L)) (car L))
    (else (find-if p (cdr L))) ) )