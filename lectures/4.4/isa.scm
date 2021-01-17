;; this loops.

(aa '(rule (is-a ?general ?specific) (and
				      (is-a ?type ?specific)
				      (is-a ?general ?type))))


(aa '(is-a thing food))
(aa '(is-a food pizza))
(aa '(is-a pizza pizza-23))

#| why does this loop?

query==> (is-a thing pizza-23)

in fact, we even get a loop for

query==> (is-a pizza pizza-23), though that first prints 1 answer.

Why:  there is one way of satisfying  (is-a pizza pizza-23),
direct from the data base.  Is there another way?  check out the
rule
  (is-a ?g ?s).  This is true if both
     (is-a ?t ?s) (is-a ?g ?t)  can be satisfied with ?g=pizza, ?s=pizza-23

So next we look for
  (is-a ?t pizza-23)  ....................[and maybe later, (is-a pizza ?t)]

this can be satisfied directly from the database (with ?t = pizza), but
there may be another way by the rule...

  (is-a ?g ?s). ...
so we are  looking for  (is-a ?g pizza-23)  again...  loop.

How to break the loop?  Use two relations: one is  "is-a" and the
other is a more specific "is-type" .  Is-a is the "transitive closure"
of is-type.

compare to  (is-mother mom daughter) 
        (is-ancestor ?Old ?young)

(aa '(rule (is-a ?general ?specific) (and
				      (is-type ?type ?specific)
				      (is-a ?general ?type))))

(aa '(rule (is-a ?x ?y) (is-type ?x ?y))))

(aa '(is-type thing food))
(aa '(is-type food pizza))
(aa '(is-type pizza pizza-23))

(aa '(is-type color orange))
(aa '(is-type food orange))







