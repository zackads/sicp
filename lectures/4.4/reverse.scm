(aa '(rule (reverse (?a . ?x) ?y)
	   (and (reverse ?x ?z)
		(append ?z (?a) ?y) )))

(aa '(reverse () ()))







(aa '(rule (backward (?a . ?x) ?y)
	   (and (append ?z (?a) ?y)
		(backward ?x ?z) )))

(aa '(backward () ()))
