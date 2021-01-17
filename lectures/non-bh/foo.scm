(aa '(rule (lastone () ?a (?a))))

(aa '(rule (lastone (?x . ?z) ?a (?x . ?y))
	   (lastone ?z ?a ?y)))

(aa '(samelength () ()))

(aa '(rule (samelength (?a . ?b) (?c . ?d))
	   (samelength ?b ?d)))

(aa '(rule (reverse (?a . ?x) (?b . ?y))
	   (and (samelength ?x ?y)
		(lastone ?z ?a (?b . ?y))
		(reverse ?x ?z) )))

(aa '(reverse () ()))
