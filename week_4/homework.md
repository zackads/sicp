# CS61A - Week 4 Homework

## Question 1

**SICP Exercise 2.7** - Alyssa’s program (section 2.1.4) is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:

```scheme
(define (make-interval a b) (cons a b))
```

Define selectors upper-bound and lower-bound to complete the implementation.

> See `interval.scm`
>
> ```scheme
> (define (make-interval a b) (cons a b))
>
> ; Assuming a is always the lower-bound and b is always the upper-bound
> (define (upper-bound interval) (cdr interval))
> (define (lower-bound interval) (car interval))
>
> ; Assuming the arguments to make-interval are un-ordered
> (define (upper-bound2 interval) (max  (car interval) (cdr interval)))
> (define (lower-bound2 interval) (min (car interval) (cdr interval)))
> ```

**SICP Exercise 2.8** - Using reasoning analogous to Alyssa’s, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

> The minimum value the difference could be is the difference of the two lower bounds and the maximum value the difference could be is the difference of the two upper bounds.

> See `interval.scm`
>
> ```scheme
> (define (sub-interval x y)
>  (make-interval (- (lower-bound x) (lower-bound y))
>                 (- (upper-bound x) (upper-bound y))))
> ```

**SICP Exercise 2.10** - Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.

> See `interval.scm`
>
> ```scheme
> (define (spans-zero? interval) (< (lower-bound interval) 0))
>
> (define (div-interval x y)
>  (if (spans-zero? y)
>      'error
>      (mul-interval x
>                    (make-interval (/ 1.0 (upper-bound y))
>                                   (/ 1.0 (lower-bound y))))))
> ```

**SICP Exercise 2.12** - Define a constructor `make-center-percent` that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector `percent` that produces the percentage tolerance for a given interval. The center selector is the same as the one shown below:

```scheme
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
```

> See `interval.scm`
>
> ```scheme
> (define (make-center-percent center percent)
>  (define interval-width (* (* percent (/ center 100)) 2))
>  (make-interval (- center interval-width)
>                 (+ center interval-width)
>
> (define (percent interval)
>  (* (/ (- (upper-bound interval) (center interval)) (center interval)) 100.0))
> ```

**SICP Exercise 2.17** - Define a procedure `last-pair` that returns the list that contains only the last element of a given (nonempty) list:

```scheme
(last-pair (list 23 72 149 34))
(34)
```

> See `last-pair.scm`
>
> ````scheme
> (define (last-pair l)
>  (if (equal? (cdr l) '())
>      l
>      (last-pair (cdr l))))
>      ```
> ````

**SICP Exercise 2.20** - The procedures `+`, `*`, and `list` take arbitrary numbers of arguments. One way to define such procedures is to use `define` with _dotted-tail notation_. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter's value will be a _list_ of any remaining arguments. For instance, given the definition

```scheme
(define (f x y . z) <body>)
```

the procedure `f` can be called with two or more arguments.

If we evaluate

```scheme
(f 1 2 3 4 5 6)
```

then in the body of `f`, `x` will be 1, `y` will be 2, and `z` will be the list `(3 4 5 6)`. Given the definition

```scheme
(define (g . w) <body>)
```

the procedure `g` can be called with zero or more arguments.

If we evaluate

`(g 1 2 3 4 5 6)`

then in the body of `g`, `w` will be the list `(1 2 3 4 5 6)`.

Use this notation to write a procedure `same-parity` that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

```scheme
(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)
(same-parity 2 3 4 5 6 7)
(2 4 6)
```

**SICP Exercise 2.22** - Louis Reasoner tries to rewrite the first `square-list` procedure of Exercise 2.21 so that it evolves an iterative process:

```scheme
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
```

Unfortunately, defining `square-list` this way produces the answer list in the reverse order of the one desired. Why?

> Because the `answer` accumulator is the second argument to the inside calls of `iter`, new square values will be added at the start of the list. See the trace:
>
> ```scheme
> >(square-list '(2 4 6 8))
> >(iter '(2 4 6 8) '())
> >(iter '(4 6 8) '(4))
> >(iter '(6 8) '(16 4))
> >(iter '(8) '(36 16 4))
> >(iter '() '(64 36 16 4))
> <'(64 36 16 4)
> ```

Louis then tries to fix his bug by interchanging the arguments to cons:

```scheme
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
```

This doesn’t work either. Explain.

> Because the `cons` function creates a pair, its use in adding to lists is taking a single value (the first argument) and pairing it to the second is that the second argument points to a list. If the first argument is itself a list and the second argument a value, it'll create a pair of a list and a value, not a value and a pointer to the next value in the list.
>
> See the trace:
>
> ```scheme
> >(square-list '(2 4 6 8))
> >(iter '(2 4 6 8) '())
> >(iter '(4 6 8) '(() . 4))
> >(iter '(6 8) '((() . 4) . 16))
> >(iter '(8) '(((() . 4) . 16) . 36))
> >(iter '() '((((() . 4) . 16) . 36) . 64))
> <'((((() . 4) . 16) . 36) . 64)
> '((((() . 4) . 16) . 36) . 64)
> ```

**SICP Exercise 2.23** - The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all — for-each is used with procedures that perform an action, such as printing. For example,

```scheme
(for-each (lambda (x)
            (newline)
            (display x))
        (list 57 321 88))
57
321
88
```

The value returned by the call to `for-each` (not illustrated above) can be something arbitrary, such as true. Give an implementation of `for-each`.

> See `for-each.scm`
>
> ```scheme
> (define (for-each function sequence)
>  (cond ((empty? sequence) #t)
>        (else (function (car sequence))
>              (for-each function (cdr sequence)))))
> ```

## Question 2

Write a procedure `substitute` that takes three arguments: a list, an old word, and a
new word. It should return a copy of the list, but with every occurrence of the old word
replaced by the new word, even in sublists. For example:

```scheme
> (substitute ’((lead guitar) (bass guitar) (rhythm guitar) drums)
              ’guitar ’axe)
((lead axe) (bass axe) (rhythm axe) drums)
```
