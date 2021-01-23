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
>      (combiner (term a) (accumulate * 1 term (next a) next b)) ))
>
> (define (product term a next b)
>  (accumulate * 1 (lambda (x) x) a next b))
>
> (define (factorial n)
>  (accumulate * 1 (lambda (x) x) 1 1+ n))
> ```
