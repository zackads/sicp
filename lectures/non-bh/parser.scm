;;by the way...

;; bad idea  (possess (person jan) (thing pizza-21))


;;  (s ?lisp ?pascal)

(aa '(rule  (S(+ ?x ?y) (?x2 + ?y2)) (and (S ?x ?x2) (S ?y ?y2))))
(aa '(rule  (S(* ?x ?y) (?x * ?y)))) ;etc
(aa '(rule (S(if ?a ?b) (if ?a then ?b)))) ;;etc ... these are half-baked
(aa '(rule (S(if ?a ?b ?c) (if ?a then ?b else ?c))))
(aa '(rule (S(set! ?a ?v) (?a := ?b)) (S ?v ?b)))

(aa '(rule (S ?a ?a)(lisp-value atom? ?a))) ;;left-overs

;; Also need something more subtle for begin a; b; c end <-> (sequence a b c)

;;;  first, a $ terminated list
;; use $ instead of comma or semicolon for now

(aa '(rule (ctl (?x . ?b) (?y $ . ?c)) (and (S ?x ?y)(ctl ?b ?c))))
(aa '(rule (ctl ()())))

(aa '(rule (S (sequence . ?a) (begin . ?g))
	   (ctl ?a ?g)))

(aa '(rule (append (?u . ?v) ?y (?u . ?z))
	   (append ?v ?y ?z)))

(aa '(rule (append () ?y ?y)))


;;now try
;;(s (sequence (set! a (+ d (* e f))) (set! b 3)) ?pascal)
;;(s ?lisp  (begin (a := (d + (e * f))) $ (b := 3) $))
