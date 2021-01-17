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

> The `else-clause` of the `new-if` procedure is always evaluated by the Scheme interpreter, regardless of the value of the predicate. `sqrt-iter` therefore calls itself for all values of `good-enough?`, resulting in an infinite loop.

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
