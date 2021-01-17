;; family.scm: Family tree example for query system.
;;
;; This first function does the same work as assert!, but can be
;; called from inside Scheme.  Using this we can load in the
;; assertions in this file, and then start up the query system.
;; To do this, do: 
;;
;; > (load "~cs60a/lib/logic.scm")
;; > (load "~cs60a/lectures/4.4/family.scm")  ; this file
;; > (query-driver-loop)
;; query==> (grandmother-of ?who is Mary)  ; etc.

(define (aa query)  ;; already in logic.scm now
  (add-rule-or-assertion!
    (add-assertion-body
      (query-syntax-process (list 'assert! query)))))

(aa '(mother-of Dave is Ann))
(aa '(father-of Dave is Mike))
(aa '(mother-of Beth is Ann))
(aa '(father-of Beth is Mike))
(aa '(mother-of Mike is Mary))
(aa '(mother-of Ann is Terri))

(aa '(rule (grandmother-of ?gc is ?gm)     ;;gc = grandchild; gm = grandmother
	   (and (mother-of ?parent is ?gm) ;; gm is the mother of parent
		(or (mother-of ?gc is ?parent) ;; parent is mother of gc or
		    (father-of ?gc is ?parent))))) ;; father of gc

;; Now try things like:
;;
;; query==> (grandmother-of ?who is Mary)
;; query==> (father-of ?who1 is ?who2)
;; query==> (?what dave is ?who2)


;;;WRONG WRONG WRONG
;;; this is nonsense query-wise

' (rule (grandma-of  ?bubu ?granny)  ;;; granny is bubu's grandma
	(mother-of ?bubu (mother-of ?mom ?granny)))
    ;; if granny is mom's mother and mom is bubu's mother.
    ;; repeat: this is nonsense.

;;;In spite of appearances, these lists  (a ?b ?c) ARE NOT FUNCTIONS
;;; but (a) queries in a data base.  They are data!!
