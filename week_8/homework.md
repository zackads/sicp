# CS61A - Week 8 Homework

## SICP Exercise 3.3

Modify the make-account procedure so that it creates password-protected accounts. That is, make-account should take a symbol as an additional argument, as in

```scheme
(define acc (make-account 100 'secret-password))
```

The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:

```scheme
((acc 'secret-password 'withdraw) 40) 60
((acc 'some-other-password 'deposit) 50) "Incorrect password"
```

> See `make-account.scm`
>
> ```scheme
> (define (make-account balance secret-password)
>  (define password secret-password)
>  (define (withdraw amount)
>    (if (>= balance amount)
>        (begin (set! balance (- balance amount)) balance)
>        "Insufficient funds"))
>  (define (deposit amount)
>    (set! balance (+ balance amount))
>    balance)
>  (define (dispatch password m)
>    (if (equal? password secret-password)
>        (cond ((eq? m 'withdraw) withdraw)
>              ((eq? m 'deposit) deposit)
>              (else (error "Unknown request: MAKE-ACCOUNT"
>                           m)))
>        (lambda (x) "Incorrect password")))
>  dispatch)
> ```

## SICP Exercise 3.4

Exercise 3.4: Modify the make-account procedure of Exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.

> See `make-account2.scm`
>
> ```scheme
> (define (make-account balance secret-password)
>  (define password secret-password)
>  (define invalid-password-attempts 0)
>  (define (withdraw amount)
>    (if (>= balance amount)
>        (begin (set! balance (- balance amount)) balance)
>        "Insufficient funds"))
>  (define (deposit amount)
>    (set! balance (+ balance amount))
>    balance)
>  (define (call-the-cops) (lambda (x) "Nee-nah-nee-nah cops are on their way!"))
>  (define (dispatch password m)
>    (if (equal? password secret-password)
>        (begin (set! invalid-password-attempts 0)
>               (cond ((eq? m 'withdraw) withdraw)
>                     ((eq? m 'deposit) deposit)
>                     (else (error "Unknown request: MAKE-ACCOUNT" m))))
>        (begin (set! invalid-password-attempts (+ invalid-password-attempts 1))
>               (if (> invalid-password-attempts 7)
>                   (call-the-cops)
>                   (lambda (x) "Incorrect password")))))
>  dispatch)
> ```

## SICP Exercise 3.7

Consider the bank account objects created by make-account, with the password modification described in Exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this.

make-joint should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. make-joint is to create an additional access to the original account using the new password.

For example, if peter-acc is a bank account with password open-sesame, then

```scheme
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
```

will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to Exercise 3.3 to accommodate this new feature.

> See `make-joint.scm`
>
> ```scheme
> (define (make-joint sub-account sub-pwd joint-pwd)
>  (define (dispatch password m)
>    (if (equal? password joint-pwd)
>        (sub-account sub-pwd m)
>        (lambda (x) "Incorrect joint password")))
>  dispatch)
> ```

## SICP Exercise 3.8

When we defined the evaluation model in Section 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left. When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result.

Define a simple procedure f such that evaluating

```
(+ (f 0) (f 1))
```

will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.

> See `f.scm`
>
> ```scheme
> (define f
>  (let ((invocations 0))
>    (lambda (x)
>      (set! invocations (+ invocations 1))
>      (cond ((= x 0) 0)
>            ((even? invocations) 0)
>            (else 1)))))
>
> (+ (f 0) (f 1)) ;; 0
> (+ (f 1) (f 0)) ;; 1
> ```

## SICP Exercise 3.10

In the `make-withdraw` procedure, the local variable `balance` is created as a parameter of `make-withdraw`. We could also create the local state variable explicitly, using `let`, as follows:

```scheme
(define (make-withdraw initial-amount) (let ((balance initial-amount))
(lambda (amount)
(if (>= balance amount)
(begin (set! balance (- balance amount)) balance)
"Insufficient funds"))))
```

Recall from Section 1.3.2 that let is simply syntactic sugar for a procedure call:

```scheme
(let ((⟨var⟩ ⟨exp⟩)) ⟨body⟩)
```

is interpreted as an alternate syntax for

```scheme
((lambda (⟨var⟩) ⟨body⟩) ⟨exp⟩)
```

Use the environment model to analyze this alternate version of `make-withdraw`, drawing figures like the ones above to illustrate the interactions

```scheme
(define W1 (make-withdraw 100)) (W1 50)
(define W2 (make-withdraw 100))
```

Show that the two versions of `make-withdraw` create objects with the same behavior. How do the environment structures differ for the two versions?

> Both versions behave the same, because the additional `(let ((balance initial-amount)) ... )` just binds the outer parameter (`initial-amount`) to the inner variable (`balance`). Nothing additional is added or removed; the two versions of `make-withdraw` behave identically.
>
> The environment structures do, however, differ. The implied lambda resulting from the `let` in the second version creates a new environment frame, in which `balance` has the value of the `initial-amount` in the parent environment. This environment slots itself in between the definition frame of `make-withdraw` and the `(lambda (amount) ...)` frame.

## SICP Exercise 3.11

In Section 3.2.3 we saw how the environment model described the behavior of procedures with local state. Now we have seen how internal definitions work. A typical message-passing procedure contains both of these aspects. Consider the bank account procedure of Section 3.1.1:

```scheme
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
           (error "Unknown request: MAKE-ACCOUNT"
                  m))))
  dispatch)
```

Show the environment structure generated by the sequence of interactions

```scheme
(define acc (make-account 50))
((acc 'deposit) 40)
90
((acc 'withdraw) 60)
30
```

Where is the local state for acc kept?

Suppose we define another account

```scheme
(define acc2 (make-account 100))
```

How are the local states for the two accounts kept distinct?

Which parts of the environment structure are shared between `acc` and `acc2`?
