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

> See `count-pairs-correct.scm`:
>
> ```scheme
> (define (count-pairs x)
> (define unique-pairs '())
> (define (count y)
> (if (or (not (pair? y)) (member y unique-pairs))
> 0
> (begin
> (set! unique-pairs (append (list y) unique-pairs))
> (+ (count (car y))
> (count (cdr y))
> 1))))
> (count x))
> ```

## SICP Exercise 3.21

Ben Bitdiddle decides to test the queue implementation described above. He types in the procedures to the Lisp interpreter and proceeds to try them out:

```scheme
(define q1 (make-queue))
(insert-queue! q1 'a) ; ((a) a)
(insert-queue! q1 'b) ; ((a b) b)
(delete-queue! q1) ; ((b) b)
(delete-queue! q1) ; (() b)
```

“It’s all wrong!” he complains. “The interpreter’s response shows that the last item is inserted into the queue twice. And when I delete both items, the second `b` is still there, so the queue isn’t empty, even though it’s supposed to be.” Eva Lu Ator suggests that Ben has misunderstood what is happening. “It’s not that the items are going into the queue twice,” she explains. “It’s just that the standard Lisp printer doesn’t know how to make sense of the queue representation. If you want to see the queue printed correctly, you’ll have to define your own print procedure for queues.”

Explain what Eva Lu is talking about. In particular, show why Ben’s examples produce the printed results that they do.

> `delete-queue!` just sets the front pointer to the second item; the front item is still there, although unavailable via the queue abstractions selectors and mutators. Print doesn't respect the queue abstraction, so it displays the raw underlying data structure.

Define a procedure `print-queue` that takes a queue as input and prints the sequence of items in the queue.

```scheme
(define print-queue front-ptr)
```

## SICP Exercise 3.25

Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The `lookup` and `insert!` procedures should take as input a list of keys used to access the table.

> Broken by unavailability of mutable pairs in Racket. See https://stackoverflow.com/questions/9475366/set-car-set-cdr-unbound-in-racket

## SICP Exercise 3.27

Memoization (also called tabulation) is a technique that enables a procedure to record, in a local table, values that have previously been computed. This technique can make a vast difference in the performance of a program. A memoized procedure maintains a table in which values of previous calls are stored using as keys the arguments that produced the values. When the memoized procedure is asked to compute a value, it first checks the table to see if the value is already there and, if so, just returns that value. Otherwise, it computes the new value in the ordinary way and stores this in the table.

As an example of memoization, recall from Section 1.2.2 the exponential process for computing Fibonacci numbers:

```scheme
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
```

The memoized version of the same procedure is:

```scheme
(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))
```

Where the memoizer is defined as:

```scheme
(define (memoize f)
(let ((table (make-table)))
(lambda (x)
(let ((previously-computed-result
(lookup x table)))
(or previously-computed-result
(let ((result (f x))) (insert! x result table) result))))))
```

Draw an environment diagram to analyze the computation of `(memo-fib 3)`.

Explain why `memo-fib` computes the nth Fibonacci number in a number of steps proportional to n. Would the scheme still work if we had simply defined `memo-fib` to be `(memoize fib)`?
