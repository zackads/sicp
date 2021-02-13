# CS61A - Week 5 Homework

## Question 1

**SICP Exercise 2.24** - Suppose we evaluate the expression `(list 1 (list 2 (list 3 4)))`. Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in Figure 2.6).

> Result from interpreter: `'(1 (2 (3 4)))`
>
> > ![Box-and-pointer diagram and tree](images/2021-02-13-12-01-37.png)

**SICP Exercise 2.26** - Suppose we define `x` and `y` to be two lists:

```scheme
(define x (list 1 2 3))
(define y (list 4 5 6))
```

What result is printed by the interpreter in response to evaluating each of the following expressions:

```scheme
(append x y)
(cons x y)
(list x y)
```

> `(append x y)` => `'(1 2 3 4 5 6)`
>
> `(cons x y)` => `'((1 2 3) 4 5 6)`
>
> `(list x y)` => `'((1 2 3) (4 5 6))`

**SICP Exercise 2.29** - A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using `list`):

```scheme
(define (make-mobile left right)
  (list left right))
```

A branch is constructed from a `length` (which must be a number) together with a `structure`, which may be either a number (representing a simple weight) or another mobile:

```scheme
(define (make-branch length structure)
  (list length structure))
```

a. Write the corresponding selectors `left-branch` and `right-branch`, which return the branches of a mobile, and `branch-length` and `branch-structure`, which return the components of a branch.

> See `mobile.scm`:
>
> ```scheme
> ; Selectors
> (define (left-branch mobile) (car mobile))
> (define (right-branch mobile) (cdr mobile))
> (define (branch-length branch) (car branch))
> (define (branch-structure branch) (cdr branch))
> ```

b. Using your selectors, define a procedure `total-weight` that returns the total weight of a mobile.

> See `mobile.scm`:
>
> ```scheme
> (define (branch-weight branch)
>  (if (number? (branch-structure branch))
>      (branch-structure branch)
>      (total-weight (branch-structure branch))))
>
> (define (total-weight mobile)
>  (+ (branch-weight (left-branch mobile))
>     (branch-weight (right-branch mobile))))
> ```

c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.

> See `mobile.scm`:
>
> ```scheme
> (define (balanced-branch? branch)
>  (if (number? (branch-structure branch))
>      #t
>      (balanced? (branch-structure branch))))
>
> (define (balanced? mobile)
>  (and (equal? (branch-torque (left-branch mobile)) (branch-torque >(right-branch mobile)))
>       (balanced-branch? (left-branch mobile))
>       (balanced-branch? (right-branch mobile))))
> ```

d. Suppose we change the representation of mobiles so that the constructors are

```scheme
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))
```

How much do you need to change your programs to convert to the new representation?

> You only need to change the selectors `right-branch` and `branch-structure` by replacing invocations of `cadr` with `cdr`. The underlying data structure is changing from a list to a pair, so `cdr` selects the second element of the pair in the same way that `cadr` selects the second element of the list. The rest of the procedures work exactly the same because they respect the abstraction barrier.
