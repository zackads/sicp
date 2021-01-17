;; append.scm: Append example for query system.
;;
;; This first function does the same work as assert!, but can be
;; called from inside Scheme.  Using this we can load in the
;; assertions in this file, and then start up the query system.
;; To do this, do: 
;;
;; > (load "~cs60a/lib/logic.scm")
;; > (load "~cs60a/lectures/4.4/append.scm")  ; this file
;; > (query-driver-loop)
;; query==> (append (1 2) ?what (1 2 3 4 5))  ; etc.

(define (aa query)
  (add-rule-or-assertion!
    (add-assertion-body
      (query-syntax-process (list 'assert! query)))))

;; we assert a rule to say that this is true:  (append (a b) (c d) (a b c d)).
;; if || means append, then V||Y = Z  implies that uV || Y = uZ
;; note that (a . (b c d e)) is the same as (a b c d e) in our data
;; language. Here's the rule (actually, it takes 2 rules).

(aa '(rule (append (?u . ?v) ?y (?u . ?z))
	   (append ?v ?y ?z)))

(aa '(rule (append () ?y ?y)))


;; compare to the usual scheme definition of append...

'(define (append a b)
   (if (null? a) b (cons (car a)(append (cdr a) b))))

;; they both have 2 cases: the base case where a=().
;;                         the recursive case where 
