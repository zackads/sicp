#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))
(require rackunit)
(require rackunit/text-ui)
(require "chatterbot.rkt")

(define file-tests
  (test-suite
    "All tests for chatterbot"

    (test-case
      "babybot"
      (check-equal?
        (babybot '(I am babybot))
        '(I am babybot)
        "test 1")
      ;; Add more tests here
    )

    (test-case
      "stupidbot-creator"
      (check-equal?
        ((stupidbot-creator '(I am Groot)) '(who are you))
        '(I am Groot)
        "test 1")
      ;; Add more tests here
    )

    (test-case
      "matcherbot-creator"
      (check-equal?
        ((matcherbot-creator '(my name is)) '(my name is starlord))
        '(starlord)
        "test 1")
      (check-equal?
        ((matcherbot-creator '(my name is)) '(the names starlord))
        #f
        "test 2")
      ;; Add more tests here
    )

    (test-case
      "substitutebot-creator"
      (check-equal?
        ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart lov happy calm delicious)) '(bad ugly stupid))
        '(good pretty smart)
        "test 1")
      ;; Add more tests here
    )

    (test-case
      "switcherbot"
      (check-equal?
        (switcherbot '(you are smart but I am smarter than you))
        '(I am smart but you are smarter than me)
        "test 1")
      ;; Add more tests here
    )

    (test-case
      "inquisitivebot"
      (check-equal?
        (inquisitivebot '(I am happy))
        '(you are happy ?)
        "test 1")
      (check-equal?
        (inquisitivebot '(I can see you))
        '(you can see me ?)
        "test 2")
      ;; Add more tests here
    )

    (test-case
      "eliza"
      (check-equal?
        (eliza '(hello))
        '(hello there!)
        "test 1")
      (check-equal?
        (eliza '(I am tired of being bullied at school))
        '(why are you tired of being bullied at school ?)
        "test 2")
      (check-equal?
        (eliza '(how are you today ?))
        '(I can not answer your question.)
        "test 3")
      (check-equal?
        (eliza '())
        '(how can I help you ?)
        "test 4")
      ;; Add more tests here
    )

    (test-case
      "reactorbot-creator"
      (check-equal?
        ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(whats up Groot)) 
        '(I am Groot)        
        "test 1")
      (check-equal?
        ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(no Groot youll die why are you doing this))
        '(WE are Groot)
        "test 2")
      ;; Add more tests here
    )

    (test-case
      "replacerbot-creator"
      (check-equal?
        ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(I dont know) '(thats nice))) '(I am) '(hi) '(im dadbot)) '(youre pretty dumb)) 
        '(thats nice)
        "test 1")
      (check-equal?
        ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(I dont know) '(thats nice))) '(I am) '(hi) '(im dadbot)) '(I am hungry))
        '(hi hungry im dadbot)
        "test 2")
      ;; Add more tests here
    )

    (test-case
      "exaggerate"
      (check-equal?
        ((exaggerate babybot 1) '(this soup is hot and tasty))
        '(this soup is very hot and very tasty)
        "test 1")
      ;; Add more tests here
    )
))

(run-tests file-tests)
