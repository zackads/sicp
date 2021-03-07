(load "obj.scm")
(load "simply.scm")

(define-class (person name)
  (method (say stuff) stuff)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name))) )

(define-class (miss-manners object)
  (method (please message argument) (ask object message argument)))

(define ZA (instantiate person 'Zack))
(define fussy-ZA (instantiate miss-manners ZA))
; (print (ask fussy-ZA 'ask '(what is the time?))) ; error
(print (ask fussy-ZA 'please 'ask '(what is the time?)))