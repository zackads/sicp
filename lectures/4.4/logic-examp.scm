;; More notes and examples on Logic programming


;;; The "."  notation


(aa '(rule (member ?a (?b . ?c)) (or (same ?a ?b)(member ?a ?c))))
(aa '(rule (same ?x ?x)))

;; works ok for (member s (r s t))
;; works ok for (member ?x (r s t))
;; has a problem with (member s ?y)
;; 
;;/////////////////

;; a rule that defines a predicate via lisp-value

;; (is-sum a b c)  which is true exactly when a+b = c.


(aa '(rule (is-sum ?x ?y ?z) (lisp-value (lambda(x y z)(= (+ x y) z))
					 ?x ?y ?z)))

;; common question from Project 4: If 
(aa '(at harry telegraph))
(aa '(possess harry book23))

;; then is book23 at telegraph?
;; Maybe..

;; If X is homeless then everything X owns is at the same location as X

(aa '(rule (at ?item ?loc)(and (possess ?who ?item)
			       (at ?who ?loc)
			       (homeless ?who))))


;; Some failures or difficulties in this model


;; If X is a student then everything X owns is in his/her dorm room
;; or in his/her car

(aa '(rule (at ?item ?room) (and (owns ?who ?item)
				(student ?who)
				(lives-in-dorm ?room ?who))))

(aa '(rule (at ?item ?car) (and (owns ?who ?item)
				(student ?who)
				(car ?car)
				(owns ?who ?car)
				)))

;; What happens if a student owns a car and lives in a dorm room?
;; All the possessions are in both places??

;;If you live in Berkeley or you are a student at Cal,
;;you root for the Cal football team.

(aa '(rule (bear-backer ?who)
	   (or (lives ?who Berkeley) (student-at ?who UCB))))

;;What about --
;; If you root for Berkeley you are a student at Cal or you live in Berkeley.

;; You might think this would work..

(aa '(rule  (or (lives ?who Berkeley) (student-at ?who UCB))
	    (bear-backer ?who)))

;; but it doesn't. You can't have a conclusion of such a complicated
;; form.  Technically, you have gone out of the subset of logic known
;; as "Horn Form" clauses and Logic Programming does not support this.

;; The success in Prolog using Horn Form clauses is basically a tribute
;; to how much can be done with just this limited logic.


;; Dealing with Negation

;;All someone owns is at his/her location unless he/she live in a dorm

(aa '(rule (at ?item ?loc)
	   (and (owns ?item ?who)
		(at ?who ?loc)
		(not(live-in-dorm ?who)))))

 Operationally, this means that when the system is trying to find out
whether (at jans-cs60a-notebook peoples-park).
the system first looks for that fact explicitly.

Failing to find that, it looks for rules by which it might conclude
(at jans-cs60a-notebook peoples-park).

These rules might be ones that conclude

(at jans-cs60a-notebook ?x)
(at ?x peoples-park)
   or
(at ?x ?y)   -- just like the one above.

In order to prove (at ?item ?loc)  we
"bind" ?item = jans-cs60a-notebook
       ?loc  = peoples-park

In general we maintain a "stream" of bindings -- an implementation
detail that is neat but inessential--- 
and then must prove all of the clauses in the "and" that follows.
(if we had an "or", we would have to prove just one of them).
In this case we must prove, with those bindings...
  
           (and (owns ?item ?who)
		(at ?who ?loc)
		(not(live-in-dorm ?who)))


or  (and (owns jans-cs60a-notebook ?who)
	 (at ?who peoples-park)
	 (not (live-in-dorm ?who)))

If we find, in our database, (owns jans-cs60a-notebook jan) we then "bind"
?who = jan in our stream of bindings and try to prove (at jan peoples-park).
If that succeeds, we use a NOT FILTER to filter out from the ?who stream,
all those items that fail to satisfy (live-in-dorm ?who)

  << THIS IS THE MEANING OF NOT IN LOGIC PROGRAMMING>>

That is, if we find  (live-in-dorm jan)  WE FAIL TO MATCH.

If we succeed at all of this, jan's notebook is at peoples-park .

If we put the clauses in the reverse order

           (and (not(live-in-dorm ?who))
		(owns ?item ?who)
		(at ?who ?loc))

This doesn't work because "not" has nothing to gnaw on.
...........
A simpler example:

(aa '(verb (hits)))
(aa '(noun (boy)))

query ==> (and (noun ?x) (not (verb ?x)))
answers:    (and (noun (boy)) (not (verb (boy)))) 

but
query==> (and (not (verb ?x))(noun ?x))
<nothing>
...........


Closed world assumption.  Say that the system concludes that
Jan's notebook is not in Peoples Park.  All it really has concluded
is that it is UNABLE TO PROVE OTHERWISE.



Other defects of (our, baby, logic system) that have been "remedied"
by more "professional" systems

1. We are unable to "retract" facts
2. Lack of any control structures  (Prolog has "cut")
[comparable in effect to introducing assignment to a
 functional language]
3. We have no input or output
4. Our system is rather inefficient (though for your examples, it is
   certainly fast enough)

Some defects are hard to remedy, and are more often dealt with
as problems in artificial intelligence "Knowledge Representation"
rather than as merely programming languages.

5. There is the illusion that we have embodied a lot more of human
common sense and reasoning than is really the case.  It is far from
true that we can deal with all declarative knowledge.



Probabilistic reasoning:
There is a 25% chance of rain tomorrow.
There is a 90% chance that the weather prediction for tomorrow is correct.

Most children learn to read by age six.

(What do ANY of those words "mean"?)
