;;; aa adds a rule or assertion to the database - makes life easier
(define (aa query)
  (add-rule-or-assertion!
    (add-assertion-body
      (query-syntax-process (list 'assert! query)))))

(aa '(rule (append (?u . ?v) ?y (?u . ?z))
	   (append ?v ?y ?z)))

(aa '(rule (append () ?y ?y)))
