;;~cs60a/lectures/2.3/trydata.scm ; revised 3/97

;; Conjugating verbs example...  

We could start with

(define (tps v); third person singular version of verb v
  (cond((equal? v 'eat) 'eats)  ; he eats
       ((equal? v 'drink) 'drinks)
       ((equal? v 'go)  'goes)
;....
       (else (error "unknown verb"))))

We need a total of six programs. Maintaining this would be really terrible.


Let us take a really crude first approach with tables.
(These are sometimes referred to as properties... e.g.
put the 'fps property of the 'eat word to ....)
 We could do this:

(put 'eat 'fps 'eat)  ; I eat    (get 'eat 'fps) --> eat
(put 'eat 'sps 'eat)  ; You eat
(put 'eat 'tps 'eats) ; He eats  (get 'eat 'tps) -->  eats
(put 'eat 'fpp 'eat)  ; We eat
(put 'eat 'spp 'eat)  ; You eat
(put 'eat 'tps 'eat)  ; They eat

;;; etc for all other verbs.

then
(define (tps verb)  (get verb 'tps))
(define (sps verb)  (get verb 'sps)) ;etc. for 4 more

The advantage here is that we can add new verbs and not rewrite programs.

The problems:  this wastes a lot of table space and still not very robust.
(tps 'nonsense) --> ()  (no error message)

more robust six programs, each looking like this:

(define (tps verb)(let((answer (get verb 'tps)))
		    (if  answer answer  ;not #f
			(error 'tps "bad verb " verb))))

Now, to reduce space consumption....

solution:  group verbs by type
                      "regular"  (just add -s in tps)  as eat, drink
                           -es       (just add -es in tps) as go, box
                      "irregular" (refer to list) be: (am are is are are are)

Then all we have to do is say that a verb is regular, and all its forms
are known.  We store most of the info on the TYPE not the data. Here's how:

(put 'eat 'verb-type 'regular)

(define (type-of-verb v) (get v 'verb-type))

(put 'fps 'regular root)  ; I eat, drink
(put 'sps 'regular root)  ; you eat, drink
(put 'tps 'regular (lambda(x)( add-s (root x))))  ; he eats, drinks
(put 'fpp 'regular root)  ; We eat, drink
etc.


Now we can write 

(define (tps verb) ;; third person singular
  (let ((proc (get 'tps (type-of-verb verb) )))

   ;; either proc is #f  i.e. not found  or we can
   ;; apply it to the root  verb:

    (if proc  (proc verb))
	(error 'tps "I don't know about verb  " verb))))

we need a total of six such programs..

...

How about using manifest types?  Earlier we had proposed (see data.scm)
that, instead of have the verb
be just 'eat, with a property  (get 'eat 'verb-type) -->  'regular

We talk about the verb as an abstraction with a type and a root,
and in the case of an irregular verb, contents that give specific
forms for the verb conjugation.  Although we cover it over with
abstraction: make-verb, type, root, contents  (see data.scm)  the verbs
look like this, if we print them as normal lisp:

'(regular . eat)   (that is, (cons 'regular  'eat))

and for an irregular verb we have '(irreg . (be am are is))
  {this actually prints as (irreg be am are is) }

where 
      (contents verb) is  (cdr verb) which is either (root) or (fp sp tp)

      (root (contents verb)) --> eat, drink etc.  (i.e. cdr)

      (type verb) is (car verb) --> regular, irregular, -es  (i.e. car)

Now with this abstraction, we have a similar program, but the
"type" program looks at the manifest type (car) rather than in a table.

(define (tps verb) ;; third person singular
  (let ((proc (get (type verb) 'tps)))  ;; this type = car gets manifest type

   ;; either proc is ()  i.e. not found  or we can
   ;; apply it to verb:

    (if (not (null? proc)) (proc verb))
	(error 'tps "I don't know about verb ~s~% " verb)))

for a total of 6 programs that are all about the same.. e.g.

(define (tpp verb) ;; third person singular
  (let ((proc (get (type verb) 'tpp)))
    (if (not (null? proc)) (proc verb))
	(error 'tpp "I don't know about verb ~s~% " verb)))

...........

Now, another simplification... Since fps, sps, tps, fpp, spp, tpp
are all almost the same program...

Why not do this: 

 (define (fps verb) (operate 'fps obj))
 (define (sps verb) (operate 'sps obj))
 (define (tps verb) (operate 'tps obj))
etc...

where operate is
(define (operate op obj)  ;; op is one of fps, sps, tps, fpp, spp, tpp
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))
        (proc (contents obj))
        (error op "Undefined operator" (list op obj)))))

;;;;;;;;;;;;;;;;

;; some detailed notes on data.scm -- how lisp represents them

;; notes 

;; note that (cons 'a 'b)  is  (a . b)   but
;;           (cons 'a (cons 'b ())  is (a b)  ...

box -->  (es . box)
          ^     ^
    type- |     |-root

(get 'es 'fps) --> procedure   (actually, root)
(get 'es 'tps) --> procedure   (actually (lambda(x)(add-es (root x))))

(operate 'fps box) --> ((get (type box) 'fps) then apply it to "box" --> box
(operate 'tps box) --> ((get (type box) 'fps) then apply it to "box" --> boxes

(define be (make-irreg 'am 'are 'is))
be --> (irreg am are is)
          ^    ^  ^   \tps
    type- |    |  \sps
               |-fps





