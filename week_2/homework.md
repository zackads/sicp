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
