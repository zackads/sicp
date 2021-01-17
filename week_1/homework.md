# CS61A - Week 1 Homework

## Question 1

SICP Exercise 1.6. Alyssa P. Hacker doesn't see why if needs to be provided as a special form. "Why can't I just define it as an ordinary procedure in terms of cond?" she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

```scheme
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
```

Eva demonstrates the program for Alyssa:

```scheme
(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0
```

Delighted, Alyssa uses new-if to rewrite the square-root program:

```scheme
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
```

What happens when Alyssa attempts to use this to compute square roots? Explain.

> The `else-clause` of the `new-if` procedure is always evaluated by the Scheme interpreter, regardless of the value of the predicate, because all procedure arguments are always evaluated in applicative order. `sqrt-iter` therefore calls itself for all values of `good-enough?`, resulting in an infinite loop.

## Question 2

Write a procedure squares that takes a sentence of numbers as its argument and returns a sentence of the squares of the numbers:

```scheme
    > (squares ’(2 3 4 5))
    > (4 9 16 25)
```

> See `squares.scm`:
>
> ```scheme
> (define (square n)
>  (* n n))
>
> (define (squares numbers)
>  (if (empty? numbers)
>      '()
>      (sentence (square (first numbers))
>          (squares (butfirst numbers)))))
> ```

## Question 3

Write a procedure switch that takes a sentence as its argument and returns a sentence in which every instance of the words I or me is replaced by you, while every instance of you is replaced by me except at the beginning of the sentence, where it’s replaced by I. (Don’t worry about capitalization of letters.)

Example:

```scheme
> (switch '(You told me that I should wake you up))
(i told you that you should wake me up)
```

> See `switch.scm`:
>
> ```scheme
> (define (replace w)
>   (cond ((equal? w 'i) 'you)
>         ((equal? w 'I) 'you)
>         ((equal? w 'me) 'you)
>         ((equal? w 'you) 'me)
>         (else w) ))
>
> (define (replace-you-with-i w)
>   (cond ((equal? w 'you) 'I)
>         ((equal? w 'You) 'I)
>         (else w)))
>
> (define (switch s)
>   (sentence
>     (replace-you-with-i (first s))
>     (switch-iter (butfirst s))))
>
> (define (switch-iter s)
>   (if (empty? s)
>       '()
>       (sentence
>         (replace (first s))
>         (switch-iter (butfirst s)))))
> ```

## Question 4

Write a predicate `ordered?` that takes a sentence of numbers as its argument and returns a true value if the numbers are in ascending order, or a false value otherwise.

> See `ordered?.scm`:
>
> ```scheme
> (define (ordered-pair? a b)
>   (< a b))
>
> (define (ordered? numbers)
>   (if (empty? numbers)
>       false
>       (ordered-pair? (first numbers) (first (butfirst numbers)))))
> ```

## Question 5

Write a procedure `ends-e` that takes a sentence as its argument and returns a sentence containing only those words of the argument whose last letter is E:

```scheme
> (ends-e '(please put the salami above the blue elephant))
(please the above the blue)
```

> See `ends-e.scm`:
>
> ```scheme
> (define (ends-in-e? w)
>  (equal? (last w) 'e))
>
> (define (filter predicate w)
>  (if
>   (predicate w)
>   w
>   '()))
>
> (define (ends-e s)
>  (if (empty? s)
>      '()
>      (sentence
>       (filter ends-in-e? (first s))
>       (ends-e (butfirst s)))))
> ```

## Question 6

Most versions of Lisp provide `and` and `or` procedures like the ones on page 19. In principle there is no reason why these can’t be ordinary procedures, but some versions of Lisp make them special forms. Suppose, for example, we evaluate

```scheme
(or (= x 0) (= y 0) (= z 0))
```

If `or` is an ordinary procedure, all three argument expressions will be evaluated before `or` is invoked. But if the variable `x` has the value `0`, we know that the entire expression has to be true regardless of the values of `y` and `z`. A Lisp interpreter in which `or` is a special form can evaluate the arguments one by one until either a true one is found or it runs out of arguments.

Your mission is to devise a test that will tell you whether Scheme’s `and` and `or` are special forms or ordinary functions. This is a somewhat tricky problem, but it’ll get you thinking about the evaluation process more deeply than you otherwise might.

> See `and-or.smc`:
>
> ```scheme
> ; If `or` is an ordinary function, this will result in an infinite > loop
> ; when the second condition is evaluated.
> ; If it is a special form, it'll just return true
>
> (define (test-or)
>   (or (< 9 10) (test-or)))
>
> ; If `and` is an ordinary function, this will evaluate in the > applicative
> ; order and return true even when n is a random closure, e.g. > (random 10).
>
> (define (test-and n)
>   (and (= 0 (- n n)) (= n (/ (+ n n) 2))))
> ```

Why might it be advantageous for an interpreter to treat `or` as a special form and evaluate its arguments one at a time?

> Evaluation the arguments of an `or` statement one at a time would allow the interpreter to cease further evaluation if one of the arguments evaluates `true`. For `or` statements with many arguments or with expensive arguments, this could make substantial efficiency gains.

Can you think of reasons why it might be advantageous to treat `or` as an ordinary function?

> Having special form elements of a language reduces consistency and increase the amount of syntactical features the programmer is required to learn before being proficient.
>
> By making `or` an ordinary function, complexity is reduced. Reduced complexity may result in productivity gains for programmers. These productivity gains may offset any runtime performance benefits of treating `or` as a special form.
