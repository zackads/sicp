# CS61A - Week 6 Homework

## Question 1 - Random number generator

For a statistical project you need to compute lots of random numbers in various ranges. (Recall that (random 10) returns a random number between 0 and 9.) Also, you need to keep track of how many random numbers are computed in each range. You decide to use object-oriented programming.

Objects of the class `random-generator` will accept two messages. The message `number` means “give me a random number in your range” while `count` means “how many number requests have you had?”

The class has an instantiation argument that specifies the range of random numbers for this object, so

```scheme
(define r10 (instantiate random-generator 10))
```

will create an object such that `(ask r10 ’number)` will return a random number between 0 and 9, while `(ask r10 ’count)` will return the number of random numbers r10 has created.

Define the `random-generator` class

> See `random-generator.scm`
>
> ```scheme
> (define-class (random-generator upper-bound)
>  (instance-vars (count 0))
>  (method (number)
>          (set! count (+ count 1))
>          (random upper-bound))
>  (method (count)
>          count))
>
> (define r10 (instantiate random-generator 10))
> (print (ask r10 'number)) ; random number between 0 and 9
> (print (ask r10 'count)) ; 1
> (print (ask r10 'number)) ; random number between 0 and 9
> (print (ask r10 'count)) ; 2
> (print (ask r10 'number)) ; random number between 0 and 9
> (print (ask r10 'count)) ; 3
> (print (ask r10 'number)) ; random number between 0 and 9
> (print (ask r10 'count)) ; 4
> ```

## Question 2 - Coke machine

Define the class `coke-machine`. The instantiation arguments for a `coke-machine` are the number of Cokes that can fit in the machine and the price (in cents) of a Coke:

```scheme
(define my-machine (instantiate coke-machine 80 70))
```

creates a machine that can hold 80 Cokes and will sell them for 70 cents each.

The machine is initially empty.

Coke-machine objects must accept the following messages:

`(ask my-machine ’deposit 25)` means deposit 25 cents. You can deposit several coins
and the machine should remember the total.

`(ask my-machine ’coke)` means push the button for a Coke. This either gives a `Not enough money` or `Machine empty` error message or returns the amount of change you get.

`(ask my-machine ’fill 60)` means add 60 Cokes to the machine.

Here’s an example:

```scheme
(ask my-machine ’fill 60)
(ask my-machine ’deposit 25)
(ask my-machine ’coke)
NOT ENOUGH MONEY
(ask my-machine ’deposit 25) ;; Now there’s 50 cents in there.
(ask my-machine ’deposit 25) ;; Now there’s 75 cents.
(ask my-machine ’coke)
5 ;; return val is 5 cents change.
```

You may assume that the machine has an infinite supply of change.

> See `coke-machine.scm`
>
> ```scheme
> (define-class (coke-machine capacity unit-price)
>  (instance-vars (cokes 0) (machine-money 0))
>  (method (deposit user-money)
>          (set! machine-money (+ machine-money user-money)))
>  (method (coke)
>          (cond ((< machine-money unit-price) (print "Not enough money"))
>                ((equal? cokes 0) (print "Machine empty"))
>                (else (set! machine-money (- machine-money unit-price))
>                      (set! cokes (- cokes 1))
>                      machine-money)))
>  (method (fill new-cokes)
>          (set! cokes (+ cokes new-cokes))))
> ```

## Question 3 - Deck of cards

We are going to use objects to represent decks of cards. You are given the list `ordered-deck` containing 52 cards in standard order:

```scheme
(define ordered-deck ’(AH 2H 3H ... QH KH AS 2S ... QC KC))
```

You are also given a function to shuffle the elements of a list:

```scheme
(define (shuffle deck)
  (if (null? deck)
      ’()
      (let ((card (nth (random (length deck)) deck)))
        (cons card (shuffle (remove card deck))) )))
```

A deck object responds to two messages: `deal` and `empty?`. It responds to deal by returning the top card of the deck, after removing that card from the deck; if the deck is
empty, it responds to deal by returning `()`. It responds to `empty?` by returning `#t` or `#f`, according to whether all cards have been dealt.

Write a class definition for `deck`. When instantiated, a `deck` object should contain a shuffled deck of 52 cards.

> See `deck.scm` (not run due to absence of `nth` function)
>
> ```scheme
> (define-class (deck)
>  (instance-vars (cards (shuffle ordered-deck)))
>  (method (deal)
>          (let ((top-card (car cards)))
>            (begin
>              (set! cards (cdr cards))
>              top-card)))
>  (method (empty?)
>          (null? cards)))
> ```

## Question 4 - Miss Manners

We want to promote politeness among our objects. Write a class `miss-manners` that takes an object as its instantiation argument. The new `miss-manners` object should accept only one message, namely `please`. The arguments to the `please` message should be, first,
a message understood by the original object, and second, an argument to that message.

(Assume that all messages to the original object require exactly one additional
argument.)

Here is an example using the person class from the upcoming adventure game project:

```scheme
> (define BH (instantiate person 'Brian BH-office))
> (ask BH 'go 'down)
BRIAN MOVED FROM BH-OFFICE TO SODA
> (define fussy-BH (instantiate miss-manners BH))
> (ask fussy-BH 'go 'east)
ERROR: NO METHOD GO
> (ask fussy-BH 'please 'go 'east)
BRIAN MOVED FROM SODA TO PSL
```

> See `miss-manners.scm`
>
> ```scheme
> (define-class (miss-manners object)
>  (method (please message argument) (ask object message argument)))
> ```
