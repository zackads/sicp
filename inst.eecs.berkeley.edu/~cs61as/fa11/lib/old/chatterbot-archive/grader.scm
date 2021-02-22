(load "chatterbot.scm")

;; Don't worry about how this function works. All you have to know
;; is its domain and range
(define (run-test test-num func-name actual-result expected-result)
  (format #t "Testing case ~A for ~A: " test-num func-name)
  (if (not (equal? expected-result actual-result))
      (format #t "Failed.~%Expected: ~A~%Got: ~A~%~%"
              expected-result actual-result)
      (format #t "Passed!~%")))

;; Tests for babybot
(define (test-babybot)
  (run-test 1 "babybot"
            (babybot '(i am babybot))
	    '(i am babybot))
  ;; Add more tests here
)
(test-babybot)

;; Tests for stupidbot-creator
(define (test-stupidbot-creator)
  (run-test 1 "stupidbot-creator"
            ((stupidbot-creator '(I am Groot)) '(who are you))
            '(I am Groot))
  ;; Add more tests here
)
(test-stupidbot-creator)

;; Tests for matcherbot-creator
(define (test-matcherbot-creator)
  (run-test 1 "matcherbot-creator"
            ((matcherbot-creator '(my name is)) '(my name is starlord))
            '(starlord))
  (run-test 2 "matcherbot-creator"
            ((matcherbot-creator '(my name is)) '(the names starlord))
            #f)
  ;; Add more tests here
)
(test-matcherbot-creator)

;; Tests for substitutebot-creator
(define (test-substitutebot-creator)
  (run-test 1 "substitutebot-creator"
            ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart lov happy calm delicious)) '(bad ugly stupid))
            '(good pretty smart))
  ;; Add more tests here
)
(test-substitutebot-creator)

;; Tests for switcherbot
(define (test-switcherbot)
  (run-test 1 "switcherbot"
            (switcherbot '(you are smart but i am smarter than you))
            '(i am smart but you are smarter than me))
  ;; Add more tests here
)
(test-switcherbot)

;; Tests for inquisitivebot
(define (test-inquisitivebot)
  (run-test 1 "inquisitivebot"
            (inquisitivebot '(i am happy))
            '(you are happy ?))
  (run-test 2 "inquisitivebot"
            (inquisitivebot '(i can see you))
            '(you can see me ?))
  ;; Add more tests here
)
(test-inquisitivebot)

;; Tests for eliza
(define (test-eliza)
  (run-test 1 "eliza"
            (eliza '(hello))
            '(hello there!))
  (run-test 2 "eliza"
            (eliza '(i am tired of being bullied at school))
            '(why are you tired of being bullied at school ?))
  (run-test 3 "eliza"
            (eliza '(how are you today ?))
            '(i can not answer your question.))
  (run-test 4 "eliza"
            (eliza '())
            '(how can i help you ?))
  ;; Add more tests here
)
(test-eliza)

;; Tests for reactorbot-creator
(define (test-reactorbot-creator)
  (run-test 1 "reactorbot-creator"
            ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(whats up Groot)) 
            '(I am Groot))
  (run-test 2 "reactorbot-creator"
            ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(no Groot youll die why are you doing this))
            '(WE are Groot))
  ;; Add more tests here
)
(test-reactorbot-creator)

;; Tests for replacerbot-creator
(define (test-replacerbot-creator)
  (run-test 1 "replacerbot-creator"
            ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(i dont know) '(thats nice))) '(i am) '(hi) '(im dadbot)) '(youre pretty dumb)) 
            '(thats nice))
  (run-test 2 "replacerbot-creator"
            ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(i dont know) '(thats nice))) '(i am) '(hi) '(im dadbot)) '(i am hungry))
            '(hi hungry im dadbot))
  ;; Add more tests here
)
(test-replacerbot-creator)

;; Tests for eliza2
(define (test-eliza2)
  (run-test 1 "eliza2"
            (eliza2 '(hello))
            '(hello there!))
  (run-test 2 "eliza2"
            (eliza2 '(i am tired of being bullied at school))
            '(why are you tired of being bullied at school ?))
  (run-test 3 "eliza2"
            (eliza2 '(how are you today ?))
            '(i can not answer your question.))
  (run-test 4 "eliza2"
            (eliza2 '())
            '(how can i help you ?))
  ;; Add more tests here
)
(test-eliza2)

;;; Keeps the old definition of random, and overwrite it afterwards
(define old-random random)
(define (random n) 0)
;; Tests for randombot-creator
(define (test-randombot-creator1)
  (run-test 1 "randombot-creator"
      ((randombot-creator (lambda (x) '(black)) (lambda (x) '(white)))
        '(blah blah blah))
      '(black))
)
(test-randombot-creator1)

(define (random n) 1)
(define (test-randombot-creator2)
  (run-test 2 "randombot-creator"
	    ((randombot-creator (lambda (x) '(black)) (lambda (x) '(white)))
	     '(blah blah blah))
	    '(white))
  )
(test-randombot-creator2)
;;; Restore the old definition of random
(define random old-random)
  
;; Tests for exaggerate
(define (test-exaggerate)
  (run-test 1 "exaggerate"
      ((exaggerate babybot 1) '(this soup is hot and tasty))
      '(this soup is very hot and very tasty))
  
  ;; Add more tests here
)
(test-exaggerate)
