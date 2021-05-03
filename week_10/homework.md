# CS61A - Week 10 Homework

1. Invent the capability to send a message to a list of clients as well as to a single client. Do this entirely in the client program, so what actually goes to the server is multiple requests.

```scheme
(define (im-many who message)
    (if (null? who)
        #f
        (begin
            (im (car who) message)
            (im-many (cdr who) message)) ))
```

2. Invent the capability to broadcast a message to every client. Do this by inventing a broadcast command that the server understands.

> Add a condition to the cond statement in `client-request-handler` such as `((equal? 'broadcast (request-action req))` that, when true, recurses over the list of clients to which the message should be broadcast and sends the `send-msg` message.

3. Could #1 have been done with the server doing part of the work? Could #2 have been done entirely in the client code? Compare the virtues of the two approaches.

1. Invent the capability of refusing messages from specific people. The sender of a refused
   message should be notified of the refusal. Decide whether to do it entirely in the client or
   with the server’s cooperation, and explain why.

1. Why is the 3-way handshake necessary when connecting to the server?

## SICP Exercises
