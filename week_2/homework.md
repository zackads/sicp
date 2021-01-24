# CS61A - Week 2 Homework

## Question 1

**SICP Exercise 1.31(a)** - The `sum` procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analagous procedure called `product` that returns the product of the values of a function at points over a given range.

> See product.scm
>
> ```scheme
> (define (product fn a b)
>  (if (> a b)
>      1
>      (* (fn a) (product fn (+ a 1) b)) ))
> ```

Show how to define `factorial` in terms of `product`.

> See factorial.scm
>
> ```scheme
> (define (factorial n)
>  (product (lambda (x) (* x 1)) 1 n))
> ```

Also use `product` to compute approximations to Pi using the formula

```
Pi / 4 = (2 * 4 * 4 * 6 * 6 * 8 ...) / (3 * 3 * 5 * 5 * 7 * 7 ...)
```

> See `pi-approx.scm`
>
> ```scheme
> (define (product fn a b)
>  (if (> a b)
>      1
>      (* (fn a)
>         (product fn (+ a 1) b))))
>
> (define (factorial n)
>  (product (lambda (x) (* x 1)) 1 n))
>
> (define (even? n)
>  (and (= (modulo n 2) 0) (not (= n 1)) ))
>
> (define (pi-approx-numerator n)
>  (cond ((eq? n 1) (+ n 1))
>        ((even? n) (+ n 2))
>        (else (+ n 1)) ))
>
> (define (pi-approx-denominator n)
>  (cond ((not (even? n)) (+ n 2))
>        (else (+ n 1)) ))
>
> (define (pi-approx)
>  (* 4.0 (/
>        (product pi-approx-numerator 1 1000)
>        (product pi-approx-denominator 1 1000)) ))
> ```

**SICP Exercise 1.32(a)** - Show that `sum` and `product` (exercise 1.31) are both special cases of a still more general notion called `accumulate` that combines a collection of terms, using some general accumulation function:

`(accumulate combiner null-value term a next b)`

`Accumulate` takes as arguments the same term and range specifications as `sum` and `product`, together with a `combiner` procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a `null-value` that specifies what base value to use when the terms run out. Write `accumulate` and show how `sum` and `product` can both be defined as simple calls to `accumulate`.

> See accumulate.scm
>
> ```scheme
> (define (1+ n) (+ n 1))
>
> (define (accumulate combiner null-value term a next b)
>  (if (> a b)
>      null-value
>      (combiner (term a)
>      (accumulate combiner null-value term (next a) next b)) ))
>
> (define (sum term a next b)
>  (accumulate + 0 term a next b))
>
> (define (product term a next b)
>  (accumulate * 1 term x a next b)
> ```

**SICP Exercise 1.33** - You can obtain an even more general version of `accumulate` (exercse 1.32) by introducing the notion of a _filter_ on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter.

Write filtered-accumulate as a procedure.

> See `filtered-accumulate.scm`
>
> ```scheme
> (define (filtered-accumulate combiner filter? null-value term a next b)
>  (define (next-match a next filter?)
>  (if (filter? (next a))
>      (next a)
>      (next-match (next a) next filter?)))
>  (if (> a b)
>      null-value
>      (combiner (term a)
>                (filtered-accumulate combiner filter? null-value term (next-match a next filter?) next b)) ))
> ```

Show how to express the following using filtered-accumulate:

a. the sum of the squares of the prime numbers in the interval _a_ to _b_ (assuming that you have a `prime?` predicate already written)

> See `filtered-accumulate.scm`
>
> ```scheme
> (define (sum-squares-primes a b)
>  (define (square n) (* n n))
>  (filtered-accumulate + prime? 0 square a 1+ b))
> ```

b. the product of all the positive integers less than _n_ that are relatively prime to `n` (i.e. all positive integers _i < n_ such that _GCD(i, n) = 1_).

> See `filtered-accumulate.scm`
>
> ```scheme
> (define (product-positive-coprimes n)
>  (define (coprime? a) (= (gcd a n) 1))
>  (filtered-accumulate * coprime? 1 (lambda (x) x) 1 1+ n))
> ```

**SICP Exercise 1.40** - Define a procedure `cubic` that can be used together with the `newtons-method` procedure in expressions of the form

`(newtons-method (cubic a b c) 1)`

to approximate zeros of the cubic _`\_x^3 + ax^2 + bx + c_

> ❌ I couldn't get my head around the mathematics of `newtons-method` and its lower-order procedures (see `cubic.scm`)to solve this one independently. From the answer sheet:
>
> ```scheme
> (define (cubic a b c)
>   (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
> ```

**SICP Exercise 1.41** - Define a procedure `double` that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if `inc` is a procedure that adds 1 to its argument, then `(double inc)` should be a procedure that adds 2. What value is returned by

`(((double (double double)) inc) 5)`

> See `double.scm`
>
> ```scheme
> (define (double procedure)
>  (lambda (x) (procedure (procedure x))))
> ```

**SICP Exercise 1.43** - If f is a numerical function and n is a posi- tive integer, then we can form the nth repeated application of f, which is defined to be the function whose value at x is f(f(...(f(x))...)). For example, if f is the function x 􏰀→ x + 1, then the nth repeated application of f is the function x 􏰀→ x +n. If f is the operation of squaring a num- ber, then the nth repeated application of f is the function that raises its argument to the 2n -th power. Write a proce- dure that takes as inputs a procedure that computes f and a positive integer n and returns the procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows:

`((repeated square 2) 5) 625`

Hint: You may find it convenient to use compose from Ex- ercise 1.42.

> See `repeated.scm`
>
> ```scheme
> (define (compose f g)
>  (lambda (x) (f (g x))))
>
> (define (repeated f n)
>  (if (eq? n 1)
>      (lambda (x) (f x))
>      (compose f (repeated f (- n 1))) ))
> ```

**SICP Exercise 1.46** - Several of the numerical methods described in this chapter are instances of an extremely general com- putational strategy known as iterative improvement. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess.

Write a procedure `iterative-improve` that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. `iterative-improve` should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough.

> See `iterative-improve.scm`
>
> ```scheme
> (define (iterative-improve good-enough? improve)
>   (define (improve-iter guess)
>     (if (good-enough? guess)
>         guess
>         (improve-iter (improve guess)) ))
>   improve-iter)
> ```

Rewrite the `sqrt` procedure of Section 1.1.7 and the `fixed-point` procedure of Section 1.3.3 in terms of iterative-improve.

> ```scheme
> (define (sqrt x)
>  (define (good-enough? guess)
>    (< (abs (- (square guess) x)) 0.001))
>  (define (improve guess)
>    (average guess (/ x guess)))
>  ((iterative-improve good-enough? improve) 1.0))
> ```
>
> ```scheme
> (define (fixed-point f first-guess)
>  (define tolerance 0.00001)
>  (define (close-enough? guess)
>    (< (abs (- (f guess) guess))
>       tolerance))
>  (define (improve guess) (f guess))
>  ((iterative-improve close-enough? improve) first-guess))
> ```
