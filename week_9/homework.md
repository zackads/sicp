# CS61A - Week 9 Homework

## SICP Exercise 3.16

Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. “It’s easy,” he reasons. “The number of pairs in any structure is the number in the `car` plus the number in the `cdr` plus one more to count the current pair.” So Ben writes the following procedure:

```scheme
(define (count-pairs x)
    (if (not (pair? x))
        0
        (+  (count-pairs (car x))
            (count-pairs (cdr x))
            1)))
```

Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben’s procedure would return 3; return 4; return 7; never return at all.

> Ben's procedure is incorrect because it double counts pairs when one pointer points to a pair that has already been counted.
>
> See `count-pairs.scm`
>
> ```scheme
> (define three-pairs1 (cons 1 (cons 2 (cons 3 4))))
>
> (define one-pair (cons 'a 'b))
> (define three-pairs2 (cons (cons one-pair one-pair) 'a))
>
> (define two-pairs (cons one-pair one-pair))
> (define three-pairs3 (cons two-pairs two-pairs))
>
> (define infinite-loop null) ; ???
>
> (count-pairs three-pairs1) ; 3
> (count-pairs three-pairs2) ; 4
> (count-pairs three-pairs3) ; 7
> (count-pairs infinite-loop) ; Didn't get this one, apparently needed to use >make-cycle
> ```

## SICP Exercise 3.17

Devise a correct version of the count-pairs procedure of Exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)

## Abelson & Sussman, exercises 3.16, 3.17, 3.21, 3.25, 3.27
