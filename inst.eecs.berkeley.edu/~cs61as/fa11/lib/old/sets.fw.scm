; Functions that implement the set-of-integers type 
; using an unordered list as the underlying representation.
; Each set thus represented has the symbol "list"
; as its first element and the set members as the
; remaining elements.

; Type name
(define list-set-type 'list)

; Return the result of forming a set out of the given elements.
(define (new-list-set elements)
  (tagged-datum list-set-type elements) )

; Return true if x is in the set, false otherwise.
(define (list-member? x set)
  (member x (contents set)) )

; Return the result of adding x to the set.
(define (list-with-element x set)
  (if (list-member? x set)
      set
      (tagged-datum list-set-type (cons x (contents set))) ) )

; Return a list of the elements of the set.
(define (list-elements set)
  (contents set) )

; *****
; Functions that implement the set-of-integers type using a 
; sorted list of intervals as the underlying representation.
; Each set thus represented has the symbol "intvls"
; as its first element, with each remaining element being a 
; two-element list representing all the integers between 
; the car of the list and the cadr, inclusive.
; The intervals of integers are sorted, e.g.
;   ((5 8) (10 10) (14 14) (100 104))
; represent the set {5, 6, 7, 8, 10, 14, 100, 101, 102, 103, 104}.
; Moreover, consecutive intervals don't overlap.

; Type name.
(define intvls-set-type 'intvls)

; Return the result of forming a set out of the given elements.
(define (new-intvls-set elements)
  ... )

; Return true if x is in the set, false otherwise.
(define (intvls-member? x set)
  ... )

; Return the result of adding x to the set.
(define (intvls-with-element x set) 
  ... )

; Return a list of the elements of the set.
(define (intvls-elements set)
  ... )

; *****
; Functions to implement tagged data.

(define (tagged-datum type value)
  (cons type value) )

(define type car)

(define contents cdr)

; *****
(define (new-set elements type)
  (cond
    ((equal? type list-set-type) (new-list-set elements))
    ((equal? type intvls-set-type) (new-intvls-set elements))
    (else (error "unknown initialization type: " type)) ) )

(define (member? x set)
  (cond
    ((list-set? set) (list-member? x set))
    ((intvls-set? set) (intvls-member? x set))
    (else (error "unknown type in member?: " (type set))) ) )

(define (with-element x set)
  (cond
    ((list-set? set) (list-with-element x set))
    ((intvls-set? set) (intvls-with-element x set))
    (else (error "unknown type in with-element: " (type set))) ) )

(define (elements set)
  (cond
    ((list-set? set) (list-elements x set))
    ((intvls-set? set) (intvls-elements x set))
    (else (error "unknown type in elements: " (type set))) ) )  

(define (list-set? set)
  (equal? (type set) list-set-type) )

(define (intvls-set? set)
  (equal? (type set) intvls-set-type) )


; *****
; Utility functions.

(define (find-if p L)
  (cond
    ((null? L) #f)
    ((p (car L)) (car L))
    (else (find-if p (cdr L))) ) )

