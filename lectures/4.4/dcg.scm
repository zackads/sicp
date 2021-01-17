(aa '(rule (S ?z)
	   (and
	    (NP ?x)
	    (VP ?y)
	    (append ?x ?y ?z))))

(aa '(rule (NP ?z)
	   (and
	    (ART ?x)
	    (NOUN ?y)
	    (append ?x ?y ?z))))

(aa '(rule (VP ?z)
	   (and
	    (VERB ?x)
	    (NP ?y)
	    (append ?x ?y ?z))))

(aa '(VERB (hits)))
(aa '(NOUN (boy)))
(aa '(ART (the)))
(aa '(NOUN (girl)))

(aa '(rule (append (?u . ?v) ?y (?u . ?z))
	   (append ?v ?y ?z)))

(aa '(rule (append () ?y ?y)))



