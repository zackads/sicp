(assert! (rule (smaller () ?x)))

(assert! (rule (smaller (a . ?x) (a . ?y))
	       (smaller ?x ?y)))

(assert! (rule (plus () ?y ?y)))

(assert! (rule (plus (a . ?x) ?y (a . ?z))
	       (plus ?x ?y ?z)))

(assert! (rule (times () ?x ())))

(assert! (rule (times (a . ?x) ?y ?z)
	       (and (smaller (a . ?x) ?z)
		    (smaller ?y ?z)
		    (times ?x ?y ?w)
		    (plus ?w ?y ?z))))

