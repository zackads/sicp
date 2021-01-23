# CS61A - Week 2 Homework

## Question 1

**SICP Exercise 1.31(a)** - The `sum` procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analagous procedure called `product` that returns the product of the values of a function at points over a given range.

```scheme
(define (product fn a b)
  (if (> a b)
      1
      (* (fn a) (product fn (+ a 1) b)) ))
```

Show how to define `factorial` in terms of `product`.

```scheme
(define (factorial n)
  (product (lambda (x) (* x 1)) 1 n))
```

Also use `product` to compute approximations to Pi using the formula

```
Pi / 4 = (2 * 4 * 4 * 6 * 6 * 8 ...) / (3 * 3 * 5 * 5 * 7 * 7 ...)
```

```scheme
(define (product fn a b)
  (if (> a b)
      1
      (* (fn a)
         (product fn (+ a 1) b))))

(define (factorial n)
  (product (lambda (x) (* x 1)) 1 n))

(define (even? n)
  (and (= (modulo n 2) 0) (not (= n 1)) ))

(define (pi-approx-numerator n)
  (cond ((eq? n 1) (+ n 1))
        ((even? n) (+ n 2))
        (else (+ n 1)) ))

(define (pi-approx-denominator n)
  (cond ((not (even? n)) (+ n 2))
        (else (+ n 1)) ))

(define (pi-approx)
  (* 4.0 (/
        (product pi-approx-numerator 1 1000)
        (product pi-approx-denominator 1 1000)) ))
```

SICP Exercise 1.31(b)
